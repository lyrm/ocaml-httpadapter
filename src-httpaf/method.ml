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
  | `PATCH
  ]

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

let from_local : M.t -> t = function
  | `GET -> `GET
  | `HEAD -> `HEAD
  | `POST -> `POST
  | `PUT -> `PUT
  | `DELETE -> `DELETE
  | `CONNECT -> `CONNECT
  | `OPTIONS -> `OPTIONS
  | `TRACE -> `TRACE
  | `Other "patch" -> `PATCH
  | `Other str -> `Other str

let to_string meth = to_local meth |> M.to_string

let compare a b = compare (to_string a) (to_string b)
