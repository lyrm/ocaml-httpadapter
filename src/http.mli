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
    | (* Cohttp only : *)
      `PATCH ]

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
    | `Permanent_redirect (* cohttp only *) ]

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
  val to_string : t -> string
end

(** HTTP version as defined in
    {{:https://tools.ietf.org/html/rfc7230#section-2.6} this rfc}. *)
module Version : sig
  (* *)

  type t = [ `HTTP_1_0 | `HTTP_1_1 | `Other of string ]

  val compare : t -> t -> int

  val to_string : t -> string
  (** [to_string t] returns a string from [t] following the syntax defined in
      {{:https://tools.ietf.org/html/rfc7230#section-2.6} this rfc}. *)

  val of_string : string -> t
  (** [of_string v] returns the version corresponding to [v] if [v] follows the
      syntax defined in {{:https://tools.ietf.org/html/rfc7230#section-2.6} this
      rfc} : [HTTP-Version = "HTTP" "/" 1*DIGIT "." 1*DIGIT] *)

  (** {b Invariants :}

      - [to_string (of_string v) = v]
      - [of_string (to_string t) = t] *)
end

module Header : sig
  type t
  type name = string
  type value = string

  val init : unit -> t
  val of_list : (name * value) list -> t

  (* to_list (of_list l) <> l for cohttp as the value of a same header
     are but next to each other in the resulting list and the
     different headers are in alphabetic order *)
  val to_list : t -> (name * value) list
  val add : t -> name -> value -> t
  val add_unless_exists : t -> name -> value -> t
  val add_list : t -> (name * value) list -> t
  val add_multi : t -> name -> value list -> t
  val remove : t -> name -> t
  val replace : t -> name -> value -> t
  val mem : t -> name -> bool
  val get : t -> name -> value option
  val get_multi : t -> name -> value list
  val compare : t -> t -> int
  val fold : (name -> value -> 'a -> 'a) -> 'a -> t -> 'a
  val to_string : t -> string

  (* TODO Add [map] function and choose a [iter] functions *)
  (* [iter] are incompatible definitions in cohttp and http/af :
        cohttp uses a function of type : name -> value list -> unit
        http/af uses a function of type : name -> value -> unit
  *)
end

module Body : sig
  type t =
    [ `Empty | `String of string | `Strings of string list | `Stream of stream ]

  and stream

  val of_string : string -> t
  val to_string : t -> string Lwt.t
  val of_string_list : string list -> t
end

module Request : sig
  type t = {
    headers : Header.t;
    meth : Method.t;
    resource : string;
    version : Version.t;
    body : Body.t;
  }

  (* cohttp function *)
  val uri : t -> Uri.t

  val make :
    ?version:Version.t ->
    ?headers:Header.t ->
    ?body:Body.t ->
    Method.t ->
    Uri.t ->
    t
end

module Response : sig
  type t = {
    headers : Header.t;
    status : Status.t;
    version : Version.t;
    body : Body.t;
  }

  val make :
    ?version:Version.t -> ?headers:Header.t -> ?body:Body.t -> Status.t -> t
end

module Accept : sig
  type p = string * string

  type media_range =
    | MediaType of string * string
    | AnyMediaSubtype of string
    | AnyMedia

  type charset = Charset of string | AnyCharset

  type encoding =
    | Encoding of string
    | Gzip
    | Compress
    | Deflate
    | Identity
    | AnyEncoding

  (** Basic language range tag. ["en-gb"] is represented as
      [Language \["en"; "gb"\]].

      @see <https://tools.ietf.org/html/rfc7231#section-5.3.5> the
      specification. *)
  type language = Language of string list | AnyLanguage

  (** Accept-Encoding HTTP header parsing and generation *)

  type q = int
  (** Qualities are integers between 0 and 1000. A header with ["q=0.7"]
      corresponds to a quality of [700]. *)

  type 'a qlist = (q * 'a) list
  (** Lists, annotated with qualities. *)

  val qsort : 'a qlist -> 'a qlist
  (** Sort by quality, biggest first. Respect the initial ordering. *)

  val media_ranges : string option -> (media_range * p list) qlist
  val charsets : string option -> charset qlist
  val encodings : string option -> encoding qlist
  val languages : string option -> language qlist
  val string_of_media_range : media_range * p list -> q -> string
  val string_of_charset : charset -> q -> string
  val string_of_encoding : encoding -> q -> string
  val string_of_language : language -> q -> string
  val string_of_media_ranges : (media_range * p list) qlist -> string
  val string_of_charsets : charset qlist -> string
  val string_of_encodings : encoding qlist -> string
  val string_of_languages : language qlist -> string
end

module Server : sig
  type callback = Request.t -> Response.t Lwt.t

  type error =
    [ `Bad_gateway | `Bad_request | `Exn of exn | `Internal_server_error ]

  type error_callback = error -> Response.t Lwt.t

  (* To think : add type conn = IP * port
     or a counter*)

  (* For now, TCP only connection *)
  val create :
    port:int -> ?error_callback:error_callback -> callback -> unit Lwt.t

  val respond :
    ?headers:Header.t -> ?body:Body.t -> Status.t -> Response.t Lwt.t

  (* val respond_with_string :
     ?version:Version.t -> ?headers:Header.t -> string -> Status.t -> t*)
end
