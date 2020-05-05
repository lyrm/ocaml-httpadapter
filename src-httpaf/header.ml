(*{{{ Copyright (C) <2020> Carine Morel <carine@tarides.com>
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

module H = Httpaf.Headers

type t = H.t

type name = string

type value = string

let init () = H.empty

let of_list = H.of_list

let to_list = H.to_list

let add = H.add

let add_unless_exists = H.add_unless_exists

let add_list h l = List.fold_left (fun h (k, v) -> H.add h k v) h l

let add_multi h name values : H.t =
  List.fold_left (fun h value -> H.add h name value) h values

let remove = H.remove

let replace = H.replace

let mem = H.mem

let get = H.get

let get_multi = H.get_multi

let fold f init = H.fold ~f ~init

let to_string = H.to_string

let compare a b = compare (to_string a) (to_string b)
