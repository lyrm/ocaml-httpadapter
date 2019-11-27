module HttpHttpaf : Http.HTTP = struct
  module Method = struct
    module M = Httpaf.Method

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
      | `PATCH ]

    let to_local : t -> M.t = function
      | `GET -> `GET
      | `HEAD -> `HEAD
      | `POST -> `POST
      | `PUT -> `PUT
      | `DELETE -> `DELETE
      | `CONNECT -> `CONNECT
      | `OPTIONS -> `OPTIONS
      | `TRACE -> `TRACE
      | `Other str -> `Other str
      | `PATCH -> `Other "patch"

    let to_string meth = to_local meth |> M.to_string

    let compare a b = compare (to_string a) (to_string b)
  end

  module Status = struct
    module S = Httpaf.Status

    type informational =
      [ `Continue
      | `Switching_protocols
      | `Processing (* cohttp only *)
      | `Checkpoint (* cohttp only *) ]

    type successful =
      [ `OK
      | `Created
      | `Accepted
      | `Non_authoritative_information
      | `No_content
      | `Reset_content
      | `Partial_content
      | `Multi_status (* cohttp only *)
      | `Already_reported (* cohttp only *)
      | `Im_used (* cohttp only *) ]

    type redirection =
      [ `Multiple_choices
      | `Moved_permanently
      | `Found
      | `See_other
      | `Not_modified
      | `Use_proxy
      | `Switch_proxy (* cohttp only *)
      | `Temporary_redirect
      | `Resume_incomplete (* cohttp only *) ]

    type client_error =
      [ `Bad_request
      | `Unauthorized
      | `Payment_required
      | `Forbidden
      | `Not_found
      | `Method_not_allowed
      | `Not_acceptable
      | `Proxy_authentication_required
      | `Request_timeout
      | `Conflict
      | `Gone
      | `Length_required
      | `Precondition_failed
      | `Request_entity_too_large (* `Payload_too_large in httpaf *)
      | `Request_uri_too_long (* `Uri_too_long in httpaf *)
      | `Unsupported_media_type
      | `Requested_range_not_satisfiable (* `Range_not_satisfiable in httpaf *)
      | `Expectation_failed
      | `I_m_a_teapot
      | `Enhance_your_calm
      | `Unprocessable_entity (* cohttp only *)
      | `Locked (* cohttp only *)
      | `Failed_dependency (* cohttp only *)
      | `Upgrade_required
      | `Precondition_required (* cohttp only *)
      | `Too_many_requests (* cohttp only *)
      | `Request_header_fields_too_large (* cohttp only *)
      | `No_response (* cohttp only *)
      | `Retry_with (* cohttp only *)
      | `Blocked_by_windows_parental_controls (* cohttp only *)
      | `Wrong_exchange_server (* cohttp only *)
      | `Client_closed_request (* cohttp only *) ]

    type server_error =
      [ `Internal_server_error
      | `Not_implemented
      | `Bad_gateway
      | `Service_unavailable
      | `Gateway_timeout
      | `Http_version_not_supported
      | `Variant_also_negotiates (* cohttp only *)
      | `Insufficient_storage (* cohttp only *)
      | `Loop_detected (* cohttp only *)
      | `Bandwidth_limit_exceeded (* cohttp only *)
      | `Not_extended (* cohttp only *)
      | `Network_authentication_required (* cohttp only *)
      | `Network_read_timeout_error (* cohttp only *)
      | `Network_connect_timeout_error (* cohttp only *) ]

    type standard =
      [ informational | successful | redirection | client_error | server_error ]

    type t = [ `Code of int | standard ]

    let to_code : t -> int = function
      | `Continue -> 100
      | `Switching_protocols -> 101
      | `Processing -> 102
      | `Checkpoint -> 103
      | `OK -> 200
      | `Created -> 201
      | `Accepted -> 202
      | `Non_authoritative_information -> 203
      | `No_content -> 204
      | `Reset_content -> 205
      | `Partial_content -> 206
      | `Multi_status -> 207
      | `Already_reported -> 208
      | `Im_used -> 226
      | `Multiple_choices -> 300
      | `Moved_permanently -> 301
      | `Found -> 302
      | `See_other -> 303
      | `Not_modified -> 304
      | `Use_proxy -> 305
      | `Switch_proxy -> 306
      | `Temporary_redirect -> 307
      | `Resume_incomplete -> 308
      | `Bad_request -> 400
      | `Unauthorized -> 401
      | `Payment_required -> 402
      | `Forbidden -> 403
      | `Not_found -> 404
      | `Method_not_allowed -> 405
      | `Not_acceptable -> 406
      | `Proxy_authentication_required -> 407
      | `Request_timeout -> 408
      | `Conflict -> 409
      | `Gone -> 410
      | `Length_required -> 411
      | `Precondition_failed -> 412
      | `Request_entity_too_large -> 413
      | `Request_uri_too_long -> 414
      | `Unsupported_media_type -> 415
      | `Requested_range_not_satisfiable -> 416
      | `Expectation_failed -> 417
      | `I_m_a_teapot -> 418
      | `Enhance_your_calm -> 420
      | `Unprocessable_entity -> 422
      | `Locked -> 423
      | `Failed_dependency -> 424
      | `Upgrade_required -> 426
      | `Precondition_required -> 428
      | `Too_many_requests -> 429
      | `Request_header_fields_too_large -> 431
      | `No_response -> 444
      | `Retry_with -> 449
      | `Blocked_by_windows_parental_controls -> 450
      | `Wrong_exchange_server -> 451
      | `Client_closed_request -> 499
      | `Internal_server_error -> 500
      | `Not_implemented -> 501
      | `Bad_gateway -> 502
      | `Service_unavailable -> 503
      | `Gateway_timeout -> 504
      | `Http_version_not_supported -> 505
      | `Variant_also_negotiates -> 506
      | `Insufficient_storage -> 507
      | `Loop_detected -> 508
      | `Bandwidth_limit_exceeded -> 509
      | `Not_extended -> 510
      | `Network_authentication_required -> 511
      | `Network_read_timeout_error -> 598
      | `Network_connect_timeout_error -> 599
      | `Code cod -> cod

    let of_code : int -> t = function
      | 100 -> `Continue
      | 101 -> `Switching_protocols
      | 102 -> `Processing
      | 103 -> `Checkpoint
      | 200 -> `OK
      | 201 -> `Created
      | 202 -> `Accepted
      | 203 -> `Non_authoritative_information
      | 204 -> `No_content
      | 205 -> `Reset_content
      | 206 -> `Partial_content
      | 207 -> `Multi_status
      | 208 -> `Already_reported
      | 226 -> `Im_used
      | 300 -> `Multiple_choices
      | 301 -> `Moved_permanently
      | 302 -> `Found
      | 303 -> `See_other
      | 304 -> `Not_modified
      | 305 -> `Use_proxy
      | 306 -> `Switch_proxy
      | 307 -> `Temporary_redirect
      | 308 -> `Resume_incomplete
      | 400 -> `Bad_request
      | 401 -> `Unauthorized
      | 402 -> `Payment_required
      | 403 -> `Forbidden
      | 404 -> `Not_found
      | 405 -> `Method_not_allowed
      | 406 -> `Not_acceptable
      | 407 -> `Proxy_authentication_required
      | 408 -> `Request_timeout
      | 409 -> `Conflict
      | 410 -> `Gone
      | 411 -> `Length_required
      | 412 -> `Precondition_failed
      | 413 -> `Request_entity_too_large
      | 414 -> `Request_uri_too_long
      | 415 -> `Unsupported_media_type
      | 416 -> `Requested_range_not_satisfiable
      | 417 -> `Expectation_failed
      | 418 -> `I_m_a_teapot
      | 420 -> `Enhance_your_calm
      | 422 -> `Unprocessable_entity
      | 423 -> `Locked
      | 424 -> `Failed_dependency
      | 426 -> `Upgrade_required
      | 428 -> `Precondition_required
      | 429 -> `Too_many_requests
      | 431 -> `Request_header_fields_too_large
      | 444 -> `No_response
      | 449 -> `Retry_with
      | 450 -> `Blocked_by_windows_parental_controls
      | 451 -> `Wrong_exchange_server
      | 499 -> `Client_closed_request
      | 500 -> `Internal_server_error
      | 501 -> `Not_implemented
      | 502 -> `Bad_gateway
      | 503 -> `Service_unavailable
      | 504 -> `Gateway_timeout
      | 505 -> `Http_version_not_supported
      | 506 -> `Variant_also_negotiates
      | 507 -> `Insufficient_storage
      | 508 -> `Loop_detected
      | 509 -> `Bandwidth_limit_exceeded
      | 510 -> `Not_extended
      | 511 -> `Network_authentication_required
      | 598 -> `Network_read_timeout_error
      | 599 -> `Network_connect_timeout_error
      | cod -> `Code cod
  end

  module Version = struct
    module V = Httpaf.Version

    type t = [ `HTTP_1_0 | `HTTP_1_1 | `Other of string ]

    let to_string : t -> string = function
      | `HTTP_1_0 -> "HTTP/1.0"
      | `HTTP_1_1 -> "HTTP/1.1"
      | `Other s -> s

    let compare a b = String.compare (to_string a) (to_string b)

    let of_string : string -> t = function
      | "HTTP/1.0" -> `HTTP_1_0
      | "HTTP/1.1" -> `HTTP_1_1
      | s -> `Other s
  end

  module Header = struct
    module H = Httpaf.Headers

    type t = H.t

    type name = string

    type value = string

    let init () = H.empty

    let of_list = H.of_list

    let to_list = H.to_list

    let add = H.add

    let add_unless_exists = H.add_unless_exists

    let add_list = H.add_list

    let add_multi = H.add_multi

    let remove = H.remove

    let replace = H.replace

    let mem = H.mem

    let get = H.get

    let get_multi = H.get_multi

    (*let iter f = H.iter ~f:f*)

    let fold f init = H.fold ~f ~init

    let to_string = H.to_string

    let compare a b = compare (to_string a) (to_string b)
  end

  module Request = struct
    module R = Httpaf.Request

    type t = {
      headers : Header.t;
      meth : Method.t;
      resource : string;
      version : Version.t;
    }

    (* cohttp function *)
    let uri { resource; headers; meth; _ } =
      match resource with
      | "*" -> (
          match Header.get headers "host" with
          | None -> Uri.of_string ""
          | Some host ->
              let host_uri = Uri.of_string ("//" ^ host) in
              let uri = Uri.(with_host (of_string "") (host host_uri)) in
              Uri.(with_port uri (port host_uri)) )
      | authority when meth = `CONNECT -> Uri.of_string ("//" ^ authority)
      | path -> (
          let uri = Uri.of_string path in
          match Uri.scheme uri with
          | Some _ -> (
              (* we have an absoluteURI *)
                Uri.(
                match path uri with "" -> with_path uri "/" | _ -> uri) )
          | None ->
              let empty = Uri.of_string "" in
              let empty_base = Uri.of_string "///" in
              let pqs =
                match Stringext.split ~max:2 path ~on:'?' with
                | [] -> empty_base
                | [ path ] ->
                    Uri.resolve "http" empty_base (Uri.with_path empty path)
                | path :: qs :: _ ->
                    let path_base =
                      Uri.resolve "http" empty_base (Uri.with_path empty path)
                    in
                    Uri.with_query path_base (Uri.query_of_encoded qs)
              in
              let uri =
                match Header.get headers "host" with
                | None -> Uri.(with_scheme (with_host pqs None) None)
                | Some host ->
                    let host_uri = Uri.of_string ("//" ^ host) in
                    let uri = Uri.with_host pqs (Uri.host host_uri) in
                    Uri.with_port uri (Uri.port host_uri)
              in
              uri )
  end

  module Accept = Cohttp.Accept
end
