module V = Cohttp.Code

type t =
  [ `HTTP_1_0
  | `HTTP_1_1
  | `Other of string
  ]

let compare = V.compare_version

let of_string = V.version_of_string

let to_string = V.string_of_version
