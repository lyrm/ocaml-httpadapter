type 'a t = 'a Lwt.t

type ic = Lwt_io.input_channel

type oc = Lwt_io.output_channel

let return = Lwt.return

let ( >>= ) = Lwt.bind

let read_line ic = Lwt_io.read_line_opt ic

let read ic n = Lwt_io.read ?count:(Some n) ic

let write oc str = Lwt_io.write oc str

let flush oc = Lwt_io.flush oc
