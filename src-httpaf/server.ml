module S = Httpaf_lwt_unix.Server

type callback = Request.t -> Response.t Lwt.t

open Lwt

let read_body on_eof (body : [ `read ] Httpaf.Body.t) : unit =
  let rec on_read acc buff ~off:_ ~len:_ =
    let acc = acc ^ Bigstringaf.to_string buff in
    Httpaf.Body.schedule_read body ~on_eof:(on_eof_ acc) ~on_read:(on_read acc)
  and on_eof_ acc () =
    Httpaf.Body.close_reader body;
    on_eof acc
  in
  Httpaf.Body.schedule_read body ~on_eof:(on_eof_ "") ~on_read:(on_read "")

let create ~port (callback : callback) : unit Lwt.t =
  let default_error_handler (_ : Unix.sockaddr) ?request:_ error handle =
    Httpaf.(
      let message =
        match error with
        | `Exn exn -> Printexc.to_string exn
        | (#Status.client_error | #Status.server_error) as error ->
            Status.to_string error
      in
      let body = handle Headers.empty in
      Body.write_string body message;
      Body.close_writer body)
  in
  let request_handler (_sockadd : Unix.sockaddr) (reqd : Httpaf.Reqd.t) : unit =
    Httpaf.Reqd.request_body reqd
    |> read_body (fun str ->
           Lwt.async (fun () ->
               Request.from_local (`String str) (Httpaf.Reqd.request reqd)
               |> callback
               >|= fun response ->
               let body = response.body in
               let resp_loc = Response.to_local response in
               match body with
               | `String str ->
                   Httpaf.Reqd.respond_with_string reqd resp_loc str
               | _ -> failwith "TODO"))
  in

  let listen sockaddr () =
    Lwt_io.establish_server_with_client_socket sockaddr
      (S.create_connection_handler ~request_handler
         ~error_handler:default_error_handler)
    >|= fun _server -> ()
  in
  Lwt.async (listen Unix.(ADDR_INET (inet_addr_any, port)));
  let forever, _ = Lwt.wait () in
  forever
