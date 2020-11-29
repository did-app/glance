import gleam/int
import gleam/option
import gleam/string
import gleam/http.{Request, Response}

pub fn request_attributes(request: Request(a)) {
  let scheme = http.scheme_to_string(request.scheme)
  let host = request.host
  let path = request.path
  // TODO decide if empty string is a good idea when query missing
  let query = option.unwrap(request.query, "")
  let method = string.uppercase(http.method_to_string(request.method))
  assert Ok(user_agent) = http.get_req_header(request, "user-agent")
  [
    tuple("http.scheme", scheme),
    tuple("http.method", method),
    tuple("http.host", host),
    tuple("http.path", path),
    tuple("http.query", query),
    tuple("http.user_agent", user_agent),
  ]
}

pub fn response_attributes(response: Response(a)) {
  let status_code = int.to_string(response.status)
  [tuple("http.status_code", status_code)]
  // TODO set status as ok
}
