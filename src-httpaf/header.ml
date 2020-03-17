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
