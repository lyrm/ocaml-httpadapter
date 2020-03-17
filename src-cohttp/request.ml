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
      { headers; meth; resource; version; encoding = Chunked (* to correct *) }

let from_local body : R.t -> t = function
  | { headers; meth; resource; version; _ } ->
      { headers; meth; resource; version; body }

let uri t = R.uri (to_local t)

let make ?(version : Version.t = `HTTP_1_1) ?(headers = Header.init ())
    ?(body : Body.t = `Empty) (meth : Method.t) uri =
  from_local body (R.make ~version ~headers ~meth uri)
