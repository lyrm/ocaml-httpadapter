module R = Httpaf.Request

type t = {
  headers : Header.t;
  meth : Method.t;
  resource : string;
  version : Version.t;
  body : Body.t;
}

let _to_local : t -> R.t = function
  | { headers; meth; resource; version; _ } ->
      R.
        {
          headers;
          meth = Method.to_local meth;
          target = resource;
          version = Version.to_local version;
        }

let from_local body : R.t -> t = function
  | R.{ headers; meth; target; version } ->
      {
        headers;
        meth = Method.from_local meth;
        resource = target;
        version = Version.from_local version;
        body;
      }

let make ?(version : Version.t = `HTTP_1_1)
    ?(headers : Header.t = Header.init ()) ?(body : Body.t = `Empty) meth uri =
  let version = Version.to_local version in
  let meth = Method.to_local meth in
  from_local body (R.create ~version ~headers meth (Uri.to_string uri))

(* cohttp function *)
let uri { resource; headers; meth; _ } =
  match resource with
  | "*" -> (
      match Header.get headers "host" with
      | None -> Uri.of_string ""
      | Some host ->
          let host_uri = Uri.of_string ("//" ^ host) in
          let uri = Uri.(with_host (of_string "") (host host_uri)) in
          Uri.(with_port uri (port host_uri)) )
  | authority when meth = `CONNECT -> Uri.of_string ("//" ^ authority)
  | path -> (
      let uri = Uri.of_string path in
      match Uri.scheme uri with
      | Some _ -> (
          (* we have an absoluteURI *)
            Uri.(
            match path uri with "" -> with_path uri "/" | _ -> uri) )
      | None ->
          let empty = Uri.of_string "" in
          let empty_base = Uri.of_string "///" in
          let pqs =
            match Stringext.split ~max:2 path ~on:'?' with
            | [] -> empty_base
            | [ path ] ->
                Uri.resolve "http" empty_base (Uri.with_path empty path)
            | path :: qs :: _ ->
                let path_base =
                  Uri.resolve "http" empty_base (Uri.with_path empty path)
                in
                Uri.with_query path_base (Uri.query_of_encoded qs)
          in
          let uri =
            match Header.get headers "host" with
            | None -> Uri.(with_scheme (with_host pqs None) None)
            | Some host ->
                let host_uri = Uri.of_string ("//" ^ host) in
                let uri = Uri.with_host pqs (Uri.host host_uri) in
                Uri.with_port uri (Uri.port host_uri)
          in
          uri )
