module R = Httpaf.Response

type t = {
  headers : Header.t;
  status : Status.t;
  version : Version.t;
  body : Body.t;
}

let from_local body : R.t -> t = function
  | R.{ headers; status; version; _ } ->
      {
        headers;
        status = Status.from_local status;
        version = Version.from_local version;
        body;
      }

let to_local : t -> R.t = function
  | { headers; version; status; _ } ->
      R.create ~version:(Version.to_local version) ~headers
        (Status.to_local status)

let make ?(version : Version.t = `HTTP_1_1) ?(headers = Header.init ())
    ?(body : Body.t = `Empty) (status : Status.t) =
  from_local body
    (R.create ~version:(Version.to_local version) ~headers
       (Status.to_local status))
