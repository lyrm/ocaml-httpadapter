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

  and stream = unit -> raw option

  (* todo *)
  and raw = unit

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

  let make ?(version : Version.t = `HTTP_1_1) ?(headers = Header.init ())
      ?(body : Body.t = `Empty) (status : Status.t) =
    from_local body (R.make ~version ~status ~headers ())
end

(* No equivalent module in Httpaf *)
module Accept = Cohttp.Accept
