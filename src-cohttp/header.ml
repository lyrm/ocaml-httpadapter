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

module H = Cohttp.Header

type t = H.t
type name = string
type value = string

let init = H.init
let of_list = H.of_list
let to_list l = List.rev (H.to_list l)
let add = H.add
let add_unless_exists = H.add_unless_exists
let add_list = H.add_list
let add_multi = H.add_multi
let remove = H.remove
let replace = H.replace
let mem = H.mem
let get = H.get
let get_multi h n = H.get_multi h n |> List.rev
let compare = H.compare

(*let iter = H.iter*)

let fold f init t = H.fold f t init
let to_string : t -> string = H.to_string
