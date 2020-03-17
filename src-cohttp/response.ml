module R = Cohttp.Response

type t = {
  headers : Header.t;
  status : Status.t;
  version : Version.t;
  body : Body.t;
}

let from_local body : R.t -> t = function
  | { headers; status; version; _ } -> { headers; status; version; body }

let to_local : t -> R.t = function
  | { headers; status; version; _ } ->
      {
        headers;
        status;
        version;
        encoding = Cohttp.Transfer.Chunked;
        flush = false;
      }

let make ?(version : Version.t = `HTTP_1_1) ?(headers = Header.init ())
    ?(body : Body.t = `Empty) (status : Status.t) =
  from_local body (R.make ~version ~status ~headers ())
