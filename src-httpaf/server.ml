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

module S = Httpaf_lwt_unix.Server

type callback = Request.t -> Response.t Lwt.t

type error =
  [ `Bad_gateway
  | `Bad_request
  | `Exn of exn
  | `Internal_server_error
  ]

type error_callback = error -> Response.t Lwt.t

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

let default_error_callback error : Response.t Lwt.t =
  let status, body =
    match error with
    | (#Status.client_error | #Status.server_error) as error ->
        (error, Status.to_string error)
    | `Exn exn -> (`Internal_server_error, Printexc.to_string exn)
  in
  Lwt.return (Response.make ~body:(`String body) status)

(* The few next functions are here to standarize encoding mangement
   between cohttp and http/af.  *)

(* This is a function coming from [Cohttp.Header] module. *)
let parse_content_range s =
  try
    let start, fini, total =
      Scanf.sscanf s "bytes %Ld-%Ld/%Ld" (fun start fini total ->
          (start, fini, total))
    in
    Some (start, fini, total)
  with Scanf.Scan_failure _ -> None

(* Http/af does not look at "content-range" value to determine body
   encoding. [content_range_to_content_length headers] add to
   [headers] the header "content-length" with a value computed from
   "content-range" header if it is present in [headers]. This is done
   only if the headers "transfer-encoding" and "content-length" are
   not already in [headers]. *)
let content_range_to_content_length (resp : Response.t) =
  let headers = resp.headers in
  match
    (Header.get headers "transfer-encoding", Header.get headers "content-length")
  with
  | Some _, _ | _, Some _ -> resp
  | None, None -> (
      match Header.get headers "content-range" with
      | None -> resp
      | Some range_s -> (
          (* Cohttp code *)
          match parse_content_range range_s with
          | Some (start, fini, total) ->
              (* some sanity checking before we act on these values *)
              if fini < total && start <= total && 0L <= start && 0L <= total
              then
                let headers =
                  Int64.add (Int64.sub fini start) 1L
                  |> Int64.to_string
                  |> Header.add headers "content-length"
                in
                { resp with headers }
              else resp
          | None -> resp ) )

let create ~port ?(error_callback = default_error_callback)
    (callback : callback) : unit Lwt.t =
  let request_handler (_sockadd : Unix.sockaddr) (reqd : Httpaf.Reqd.t) : unit =
    Httpaf.Reqd.request_body reqd
    |> read_body (fun str ->
           Lwt.async (fun () ->
               Request.from_local (`String str) (Httpaf.Reqd.request reqd)
               |> callback
               >|= fun response ->
               let response = content_range_to_content_length response in
               let body = response.body in
               let resp_loc = Response.to_local response in
               match body with
               | `String str ->
                   Httpaf.Reqd.respond_with_string reqd resp_loc str
               | _ -> failwith "TODO"))
  in

  let error_handler (_ : Unix.sockaddr) ?request:_ error start_response =
    Lwt.async (fun () ->
        error_callback error >|= fun response ->
        let body = start_response response.headers in
        ( match response.body with
        | `String str -> Httpaf.Body.write_string body str
        (* to correct for efficiently *)
        | `Strings strs -> Httpaf.Body.write_string body (String.concat "" strs)
        | _ -> failwith "TODO" );
        Httpaf.Body.close_writer body)
  in

  let listen sockaddr () =
    Lwt_io.establish_server_with_client_socket sockaddr
      (S.create_connection_handler ~request_handler ~error_handler)
    >|= fun _server -> ()
  in
  Lwt.async (listen Unix.(ADDR_INET (inet_addr_any, port)));
  let forever, _ = Lwt.wait () in
  forever
