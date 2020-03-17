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
