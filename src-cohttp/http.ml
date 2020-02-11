(*{{{ Copyright (C) <2019> Carine Morel <carine@tarides.com>
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

open Sexplib0.Sexp_conv

module Method = struct
  module C = Cohttp.Code

  type t =
    [ `GET
    | `HEAD
    | `POST
    | `PUT
    | `DELETE
    | `CONNECT
    | `OPTIONS
    | `TRACE
    | `Other of string
    | `PATCH
    ]

  let compare = C.compare_method

  let to_string (meth : t) = C.string_of_method meth
end

module Status = struct
  module S = Cohttp.Code

  type informational = S.informational_status

  type successful = S.success_status

  type redirection = S.redirection_status

  type client_error = S.client_error_status

  type server_error = S.server_error_status

  type standard = S.status

  type t = S.status_code

  let to_code = S.code_of_status

  let of_code = S.status_of_code
end

module Version = struct
  module V = Cohttp.Code

  type t =
    [ `HTTP_1_0
    | `HTTP_1_1
    | `Other of string
    ]

  let compare = V.compare_version

  let of_string = V.version_of_string

  let to_string = V.string_of_version
end

module Header = struct
  module H = Cohttp.Header

  type t = H.t

  type name = string

  type value = string

  let init = H.init

  let of_list = H.of_list

  let to_list = H.to_list

  let add = H.add

  let add_unless_exists = H.add_unless_exists

  let add_list = H.add_list

  let add_multi (headers : t) (l : (name * value list) list) : t =
    List.fold_left (fun h (name, values) -> H.add_multi h name values) headers l

  let remove = H.remove

  let replace = H.replace

  let mem = H.mem

  let get = H.get

  let get_multi = H.get_multi

  let compare = H.compare

  (*let iter = H.iter*)

  let fold f init t = H.fold f t init

  let to_string : t -> string = H.to_string
end

module Body = struct
  module B = Cohttp.Body

  type t =
    [ `Empty
    | `String of string
    | `Strings of string list
    | `Stream of stream
    ]

  and stream = string Lwt_stream.t sexp_opaque

  let of_string s = `String s

  let of_string_list s = `Strings s
end

module Request = struct
  module R = Cohttp.Request

  type t = {
    headers : Header.t;
    meth : Method.t;
    resource : string;
    version : Version.t;
    body : Body.t;
  }

  let to_local : t -> R.t = function
    | { headers; meth; resource; version; _ } ->
        {
          headers;
          meth;
          resource;
          version;
          encoding = Chunked (* to correct *);
        }

  let from_local body : R.t -> t = function
    | { headers; meth; resource; version; _ } ->
        { headers; meth; resource; version; body }

  let uri t = R.uri (to_local t)

  let make ?(version : Version.t = `HTTP_1_1) ?(headers = Header.init ())
      ?(body : Body.t = `Empty) (meth : Method.t) uri =
    from_local body (R.make ~version ~headers ~meth uri)
end

module Response = struct
  module R = Cohttp.Response

  type t = {
    headers : Header.t;
    status : Status.t;
    version : Version.t;
    body : Body.t;
  }

  let from_local body : R.t -> t = function
    | { headers; status; version; _ } -> { headers; status; version; body }

  let to_local : t -> R.t = function
    | { headers; status; version; _ } ->
        {
          headers;
          status;
          version;
          encoding = Cohttp.Transfer.Chunked;
          flush = false;
        }

  let make ?(version : Version.t = `HTTP_1_1) ?(headers = Header.init ())
      ?(body : Body.t = `Empty) (status : Status.t) =
    from_local body (R.make ~version ~status ~headers ())
end

(* No equivalent module in Httpaf *)
module Accept = Cohttp.Accept

module Server = struct
  (* Most of the code here is inlined from Conduit library *)

  type callback = Request.t -> Response.t Lwt.t

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
        (fun exn ->
          on_exn exn;
          Lwt.return_unit)
    in
    let sockaddr = Unix.(ADDR_INET (inet_addr_any, port)) in
    listen ?backlog sockaddr >>= init ?stop (process_accept ?timeout callback)

  let create (*?timeout ?backlog ?stop ?(on_exn = fun _ -> ())*) ~port
      (callback : callback) =
    let spec =
      S.make
        ~callback:(fun _conn request body ->
          callback (Request.from_local body request) >|= fun resp ->
          (Response.to_local resp, (resp.body : Cohttp_lwt.Body.t)))
        ()
    in
    serve (*?backlog ?timeout ?stop *) ~on_exn:(fun _ -> ()) ~port (S.callback spec)
end
