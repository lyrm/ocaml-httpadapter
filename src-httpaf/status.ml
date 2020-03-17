module S = Httpaf.Status

type informational =
  [ `Continue
  | `Switching_protocols
  | `Processing (* cohttp only *)
  | `Checkpoint (* cohttp only *)
  ]

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
  | `Im_used (* cohttp only *)
  ]

type redirection =
  [ `Multiple_choices
  | `Moved_permanently
  | `Found
  | `See_other
  | `Not_modified
  | `Use_proxy
  | `Switch_proxy (* cohttp only *)
  | `Temporary_redirect
  | `Resume_incomplete (* cohttp only *)
  ]

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
  | `Client_closed_request (* cohttp only *)
  ]

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
  | `Network_connect_timeout_error (* cohttp only *)
  ]

type standard =
  [ informational
  | successful
  | redirection
  | client_error
  | server_error
  ]

type t =
  [ `Code of int
  | standard
  ]

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

let to_local : t -> S.t = function
  (* informational *)
  | (`Continue | `Switching_protocols) as s -> s
  | (`Processing | `Checkpoint) as s -> `Code (to_code s)
  (* successful *)
  | ( `OK | `Created | `Accepted | `Non_authoritative_information | `No_content
    | `Reset_content | `Partial_content ) as s ->
      s
  | (`Multi_status | `Already_reported | `Im_used) as s -> `Code (to_code s)
  (* redirection *)
  | ( `Multiple_choices | `Moved_permanently | `Found | `See_other
    | `Not_modified | `Use_proxy ) as s ->
      s
  | (`Switch_proxy | `Temporary_redirect | `Resume_incomplete) as s ->
      `Code (to_code s)
  (* client_error *)
  | ( `Bad_request | `Unauthorized | `Payment_required | `Forbidden | `Not_found
    | `Method_not_allowed | `Not_acceptable | `Proxy_authentication_required
    | `Request_timeout | `Conflict | `Gone | `Length_required
    | `Precondition_failed | `Unsupported_media_type | `Expectation_failed
    | `Upgrade_required | `I_m_a_teapot | `Enhance_your_calm ) as s ->
      s
  | `Request_entity_too_large -> `Payload_too_large
  | `Request_uri_too_long -> `Uri_too_long
  | `Requested_range_not_satisfiable -> `Range_not_satisfiable
  | ( `Unprocessable_entity | `Locked | `Failed_dependency
    | `Precondition_required | `Too_many_requests
    | `Request_header_fields_too_large | `No_response | `Retry_with
    | `Blocked_by_windows_parental_controls | `Wrong_exchange_server
    | `Client_closed_request ) as s ->
      `Code (to_code s)
  (* server_error *)
  | ( `Internal_server_error | `Not_implemented | `Bad_gateway
    | `Service_unavailable | `Gateway_timeout | `Http_version_not_supported ) as
    s ->
      s
  | ( `Variant_also_negotiates | `Insufficient_storage | `Loop_detected
    | `Bandwidth_limit_exceeded | `Not_extended
    | `Network_authentication_required | `Network_read_timeout_error
    | `Network_connect_timeout_error ) as s ->
      `Code (to_code s)
  (* other *)
  | `Code c -> `Code c

let from_local : S.t -> t = function
  (* information *)
  | (`Continue | `Switching_protocols) as s -> s
  (* successful *)
  | ( `OK | `Created | `Accepted | `Non_authoritative_information | `No_content
    | `Reset_content | `Partial_content ) as s ->
      s
  (* redirection *)
  | ( `Multiple_choices | `Moved_permanently | `Found | `See_other
    | `Not_modified | `Use_proxy | `Temporary_redirect ) as s ->
      s
  (* client_error *)
  | ( `Bad_request | `Unauthorized | `Payment_required | `Forbidden | `Not_found
    | `Method_not_allowed | `Not_acceptable | `Proxy_authentication_required
    | `Request_timeout | `Conflict | `Gone | `Length_required
    | `Precondition_failed | `Unsupported_media_type | `Expectation_failed
    | `Upgrade_required | `I_m_a_teapot | `Enhance_your_calm ) as s ->
      s
  | `Payload_too_large -> `Request_entity_too_large
  | `Uri_too_long -> `Request_uri_too_long
  | `Range_not_satisfiable -> `Requested_range_not_satisfiable
  (* server_error *)
  | ( `Internal_server_error | `Not_implemented | `Bad_gateway
    | `Service_unavailable | `Gateway_timeout | `Http_version_not_supported ) as
    s ->
      s
  | `Code n -> of_code n
