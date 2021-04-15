(*{{{ Copyright (C) <2020> Carine Morel <carine@tarides.com>
*
* Permission to use, copy, modify, and distribute this software for any
* purpose with or without fee is hereby granted, provided that the above
* copyright notice and this permission notice appear in all copies.
*
* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
}}}*)

(* Most of the code here is inlined from Conduit library *)

type callback = Request.t -> Response.t Lwt.t

type error =
  [ `Bad_gateway | `Bad_request | `Exn of exn | `Internal_server_error ]

type error_callback = error -> Response.t Lwt.t

module IO_s :
  Cohttp_lwt.S.IO
    with type ic = Lwt_io.input_channel
     and type oc = Lwt_io.output_channel
     and type conn = unit = struct
  type 'a t = 'a Lwt.t
  type error = exn
  type ic = Lwt_io.input_channel
  type oc = Lwt_io.output_channel

  let return = Lwt.return
  let ( >>= ) = Lwt.bind
  let read_line ic = Lwt_io.read_line_opt ic
  let read ic n = Lwt_io.read ?count:(Some n) ic
  let write oc str = Lwt_io.write oc str
  let flush oc = Lwt_io.flush oc
  let pp_error = Fmt.exn

  (*    exception IO_error of exn*)

  let catch f =
    Lwt.try_bind f Lwt.return_ok (function
        (*| IO_error e -> Lwt.return_error e*)
        | ex -> Lwt.fail ex)

  (* Useful without conduit  *)
  type conn = unit
end

module S = Cohttp_lwt.Make_server (IO_s)
open Lwt

(* ** Conduit code to handle a limited number of connections ** *)
let maxactive = ref None
let active = ref 0
let cond = Lwt_condition.create ()
let connected () = incr active

let disconnected () =
  decr active;
  Lwt_condition.broadcast cond ()

let rec throttle () =
  match !maxactive with
  | Some limit when !active > limit -> Lwt_condition.wait cond >>= throttle
  | _ -> Lwt.return_unit

let run_handler handler v =
  Lwt.async (fun () ->
      Lwt.try_bind
        (fun () -> handler v)
        (fun () ->
          disconnected ();
          Lwt.return_unit)
        (fun x ->
          disconnected ();
          (match x with Lwt.Canceled -> () | _ -> ());
          Lwt.return_unit))

let safe_close t =
  Lwt.catch (fun () -> Lwt_io.close t) (fun _ -> Lwt.return_unit)

let close (ic, oc) = safe_close oc >>= fun () -> safe_close ic

let set_sockopts_no_exn fd =
  try
    Lwt_unix.setsockopt fd Lwt_unix.TCP_NODELAY true
    (* Set option nodelay sur la socket fd *)
  with
  (* This is expected for Unix domain sockets *)
  | Unix.Unix_error (Unix.EOPNOTSUPP, _, _) ->
    ()

let shutdown_no_exn fd mode =
  try Lwt_unix.shutdown fd mode
  with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()

let channels_from_fd fd =
  let fd_state = ref `Open in
  (* init fd state*)
  let close_in () =
    match !fd_state with
    | `Open ->
        fd_state := `In_closed;
        shutdown_no_exn fd Unix.SHUTDOWN_RECEIVE;
        Lwt.return_unit
    | `Out_closed ->
        fd_state := `Closed;
        Lwt_unix.close fd
    | `In_closed (* repeating on a closed channel is a noop in Lwt_io *)
    | `Closed ->
        Lwt.return_unit
  in
  let close_out () =
    match !fd_state with
    | `Open ->
        fd_state := `Out_closed;
        shutdown_no_exn fd Unix.SHUTDOWN_SEND;
        Lwt.return_unit
    | `In_closed ->
        fd_state := `Closed;
        Lwt_unix.close fd
    | `Out_closed (* repeating on a closed channel is a noop in Lwt_io *)
    | `Closed ->
        Lwt.return_unit
  in
  let ic = Lwt_io.of_fd ~close:close_in ~mode:Lwt_io.input fd in
  let oc = Lwt_io.of_fd ~close:close_out ~mode:Lwt_io.output fd in
  (ic, oc)

let listen ?(backlog = 128) sa =
  let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sa) Unix.SOCK_STREAM 0 in
  Lwt.catch
    (fun () ->
      Lwt_unix.(setsockopt fd SO_REUSEADDR true);
      Lwt_unix.bind fd sa >|= fun () ->
      Lwt_unix.listen fd backlog;
      Lwt_unix.set_close_on_exec fd;
      fd)
    (fun e ->
      Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)
      >>= fun () -> Lwt.fail e)

let init ?(stop = fst (Lwt.wait ())) handler fd =
  let stop = Lwt.map (fun () -> `Stop) stop in

  let rec loop () =
    Lwt.try_bind
      (fun () ->
        connected ();
        throttle () >>= fun () ->
        let accept = Lwt.map (fun v -> `Accept v) (Lwt_unix.accept fd) in
        Lwt.choose [ accept; stop ] >|= function
        | `Stop ->
            Lwt.cancel accept;
            `Stop
        | `Accept _ as x -> x)
      (function
        | `Stop ->
            disconnected ();
            Lwt.return_unit
        | `Accept v ->
            run_handler handler v;
            (* Call the callback !*)
            loop ())
      (fun exn ->
        disconnected ();
        match exn with
        | Lwt.Canceled -> Lwt.return_unit
        | _ -> Lwt_unix.yield () >>= loop)
  in
  Lwt.finalize loop (fun () -> Lwt_unix.close fd)

let process_accept ?timeout callback (client, _peeraddr) : unit Lwt.t =
  set_sockopts_no_exn client;
  let ic, oc = channels_from_fd client in
  let c = callback () ic oc in
  let events =
    match timeout with
    | None -> [ c ] (* Gestion du timeout *)
    | Some t -> [ c; Lwt_unix.sleep (float_of_int t) ]
  in
  Lwt.finalize (fun () -> Lwt.pick events) (fun () -> close (ic, oc))

let serve ?backlog ?timeout ?stop ~on_exn ~(port : int) callback =
  let callback flow ic oc =
    Lwt.catch
      (fun () -> callback flow ic oc)
      (fun exn -> on_exn exn >>= fun () -> Lwt.return_unit)
  in
  let sockaddr = Unix.(ADDR_INET (inet_addr_any, port)) in
  listen ?backlog sockaddr >>= init ?stop (process_accept ?timeout callback)

let default_error_callback error : Response.t Lwt.t =
  let status, body =
    match error with
    | (#Status.client_error | #Status.server_error) as error ->
        (error, Status.to_string error)
    | `Exn exn -> (`Internal_server_error, Printexc.to_string exn)
  in
  Lwt.return (Response.make ~body:(`String body) status)

let create ~(*?timeout ?backlog ?stop *) port
    ?(error_callback = default_error_callback) (callback : callback) =
  let local_callback _conn request body =
    callback (Request.from_local body request) >|= fun resp ->
    (Response.to_local resp, (resp.body : Cohttp_lwt.Body.t))
  in

  let spec = S.make ~callback:local_callback () in

  (* TODO : finish error handling *)
  let on_exn exn =
    error_callback (`Exn exn) >>= fun resp ->
    Body.to_string resp.body >>= fun body -> Lwt_io.print body
  in
  serve (*?backlog ?timeout ?stop *) ~on_exn ~port (S.callback spec)

let respond ?headers ?(body = `Empty) status : Response.t Lwt.t =
  S.respond ?headers ~body ~status () >|= fun (resp, body) ->
  Response.from_local body resp
