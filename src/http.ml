module type HTTP = sig
  module Method : sig
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

    val compare : t -> t -> int

    val to_string : t -> string
  end

  module Status : sig
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

    val to_code : t -> int

    val of_code : int -> t
  end

  module Version : sig
    type t = [ `HTTP_1_0 | `HTTP_1_1 | `Other of string ]

    val compare : t -> t -> int

    val to_string : t -> string

    val of_string : string -> t
  end

  module Header : sig
    type t

    type name = string

    type value = string

    val init : unit -> t

    val of_list : (name * value) list -> t

    val to_list : t -> (name * value) list

    val add : t -> name -> value -> t

    val add_unless_exists : t -> name -> value -> t

    val add_list : t -> (name * value) list -> t

    val add_multi : t -> (name * value list) list -> t

    val remove : t -> name -> t

    val replace : t -> name -> value -> t

    val mem : t -> name -> bool

    val get : t -> name -> value option

    val get_multi : t -> name -> value list

    val compare : t -> t -> int

    (* Incompatible definitions :
       cohttp uses a function of type : name -> value list -> unit
       httpaf uses a function of type : name -> value -> unit
    val iter : (name -> value -> unit) -> t -> unit*)

    val fold : (name -> value -> 'a -> 'a) -> 'a -> t -> 'a

    val to_string : t -> string
  end

  module Request : sig
    type t = {
      headers : Header.t;
      meth : Method.t;
      resource : string;
      version : Version.t;
      (*encoding: Transfer.encoding; (** transfer encoding of this HTTP request *)*)
    }

    (* cohttp function *)
    val uri : t -> Uri.t
  end

  module Accept = Cohttp.Accept

end
