module S = Cohttp.Code

type informational = S.informational_status

type successful = S.success_status

type redirection = S.redirection_status

type client_error = S.client_error_status

type server_error = S.server_error_status

type standard = S.status

type t = S.status_code

let to_code = S.code_of_status

let of_code = S.status_of_code
