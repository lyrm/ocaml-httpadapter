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
module S = Cohttp_lwt_unix.Server
open Lwt

type callback = Request.t -> Response.t Lwt.t

type error =
  [ `Bad_gateway | `Bad_request | `Exn of exn | `Internal_server_error ]

type error_callback = error -> Response.t Lwt.t

let default_error_callback error : Response.t Lwt.t =
  let status, body =
    match error with
    | (#Status.client_error | #Status.server_error) as error ->
        (error, Status.to_string error)
    | `Exn exn -> (`Internal_server_error, Printexc.to_string exn) in
  Lwt.return (Response.make ~body:(`String body) status)

(* Improve readability by mixing properly sage/build_handler at least*)
let safe error_handler callback spec flow () =
  Lwt.catch
    (fun () ->
      let ic, oc = Conduit_lwt.io_of_flow flow in
      callback spec flow ic oc)
    (fun exn -> error_handler exn)

(* TODO : improve *)
let build_error_handler (error_callback : error_callback) : exn -> unit Lwt.t =
 fun exn ->
  error_callback (`Exn exn) >>= fun resp ->
  Body.to_string resp.body >>= fun body -> Lwt_io.print body

let build_local_callback (callback : callback) :
    _ ->
    Cohttp.Request.t ->
    Body.t ->
    (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t =
 fun _conn request body ->
  callback (Request.from_local body request) >|= fun resp ->
  (Response.to_local resp, (resp.body : Cohttp_lwt.Body.t))

let build_handler protocol error_handler spec : 'a -> unit Lwt.t =
 fun flow ->
  let flow = Conduit_lwt.pack protocol flow in
  Lwt.finalize (safe error_handler S.callback spec flow) (fun () ->
      Conduit_lwt.close flow >>= fun _ -> Lwt.return_unit)

(** Create a server with customized connection parameters using Conduit. *)
let create :
    type cfg t flow.
    ?timeout:int ->
    ?backlog:int ->
    ?stop:unit Lwt.t ->
    ?error_handler:error_callback ->
    cfg ->
    (_, flow) Conduit_lwt.protocol ->
    (cfg, t, flow) Conduit_lwt.Service.service ->
    callback ->
    unit ->
    unit Lwt.t =
 fun ?timeout ?(backlog = 128) ?(stop = fst (Lwt.wait ()))
     ?error_handler:(error_callback = default_error_callback) cfg protocol
     service callback ->
  let error_handler = build_error_handler error_callback in
  let local_callback = build_local_callback callback in
  let spec = S.make ~callback:local_callback () in

  let cfg : cfg =
    match Conduit_lwt.Service.equal service Conduit_lwt.TCP.service with
    | Some (Conduit.Refl, _, _) ->
        { cfg with Conduit_lwt.TCP.capacity = backlog }
    | None -> cfg in

  let handler = build_handler protocol error_handler spec in
  let cond, run = Conduit_lwt.serve ?timeout ~handler ~service cfg in
  (* WHY ? *)
  () ;
  fun () -> Lwt.pick [ stop >|= Lwt_condition.signal cond; run () ]

(** Create a TCP server with Conduit-lwt TCP pre-defined module *)
let create_tcp :
    ?backlog:int ->
    ?timeout:int ->
    ?stop:unit Lwt.t ->
    ?error_handler:error_callback ->
    port:int ->
    callback ->
    unit ->
    unit Lwt.t =
 fun ?(backlog = 128) ?timeout ?(stop = fst (Lwt.wait ()))
     ?error_handler:(error_callback = default_error_callback) ~port callback ->
  let error_handler = build_error_handler error_callback in
  let local_callback = build_local_callback callback in
  let spec = S.make ~callback:local_callback () in

  let protocol = Conduit_lwt.TCP.protocol in
  let service = Conduit_lwt.TCP.service in
  let cfg : Conduit_lwt.TCP.configuration =
    Conduit_lwt.TCP.
      { sockaddr = Unix.(ADDR_INET (inet_addr_any, port)); capacity = backlog }
  in

  let handler = build_handler protocol error_handler spec in
  let cond, run = Conduit_lwt.serve ?timeout ~handler ~service cfg in
  () ;
  fun () -> Lwt.pick [ stop >|= Lwt_condition.signal cond; run () ]

(* TODO check the parameter that we may want to lift here for defining
   TLS protocol *)

(** Create a TLS/TCP server with Conduit-lwt TLS and TCP predefined modules. *)
let create_tls :
    ?backlog:int ->
    ?timeout:int ->
    ?stop:unit Lwt.t ->
    ?error_handler:error_callback ->
    port:int ->
    callback ->
    unit ->
    unit Lwt.t =
 fun ?(backlog = 128) ?timeout ?(stop = fst (Lwt.wait ()))
     ?error_handler:(error_callback = default_error_callback) ~port callback ->
  let error_handler = build_error_handler error_callback in
  let local_callback = build_local_callback callback in
  let spec = S.make ~callback:local_callback () in

  let protocol_with_tls =
    Conduit_lwt_tls.protocol_with_tls Conduit_lwt.TCP.protocol in
  let service_with_tls =
    Conduit_lwt_tls.service_with_tls Conduit_lwt.TCP.service protocol_with_tls
  in
  let cfg_tls : Conduit_lwt.TCP.configuration * Tls.Config.server =
    ( Conduit_lwt.TCP.
        {
          sockaddr = Unix.(ADDR_INET (inet_addr_any, port));
          capacity = backlog;
        },
      Tls.Config.server () ) in

  let handler = build_handler protocol_with_tls error_handler spec in
  let cond, run =
    Conduit_lwt.serve ?timeout ~handler ~service:service_with_tls cfg_tls in
  () ;
  fun () -> Lwt.pick [ stop >|= Lwt_condition.signal cond; run () ]

let respond ?headers ?(body = `Empty) status : Response.t Lwt.t =
  S.respond ?headers ~body ~status () >|= fun (resp, body) ->
  Response.from_local body resp
