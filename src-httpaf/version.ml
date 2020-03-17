module V = Httpaf.Version

type t =
  [ `HTTP_1_0
  | `HTTP_1_1
  | `Other of string
  ]

let to_string : t -> string = function
  | `HTTP_1_0 -> "HTTP/1.0"
  | `HTTP_1_1 -> "HTTP/1.1"
  | `Other s -> s

let compare a b = String.compare (to_string a) (to_string b)

let of_string : string -> t = function
  | "HTTP/1.0" -> `HTTP_1_0
  | "HTTP/1.1" -> `HTTP_1_1
  | s -> `Other s

let parse_version = V.of_string

let to_local : t -> V.t = function
  | `HTTP_1_0 -> V.{ major = 1; minor = 0 }
  | `HTTP_1_1 -> V.{ major = 1; minor = 1 }
  | `Other s -> parse_version s

let from_local : V.t -> t = function
  | { major = 1; minor = 0 } -> `HTTP_1_0
  | { major = 1; minor = 1 } -> `HTTP_1_1
  | other -> `Other (V.to_string other)
