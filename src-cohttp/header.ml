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
