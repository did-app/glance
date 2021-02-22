import gleam/bit_builder.{BitBuilder}
import gleam/bit_string.{BitString}
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/uri
import gleam/http.{Get, Options, Request, Response}
import gleam/httpc
import gleam/json
import floki
import glance

pub fn set_resp_json(response, data) {
  let body =
    data
    |> json.encode
    |> bit_string.from_string
    |> bit_builder.from_bit_string

  response
  |> http.prepend_resp_header("content-type", "application/json")
  |> http.set_resp_body(body)
}

pub fn handle(request: Request(BitString), config: Nil) -> Response(BitBuilder) {
  io.debug(request)
  case request.method, http.path_segments(request) {
    Options, [] ->
      http.response(200)
      |> http.set_resp_body(bit_builder.from_bit_string(<<>>))
    Get, [] ->
      case request.query {
        Some(target) -> {
          assert Ok(uri) = uri.parse(target)
          let preview = glance.scan_uri(uri)
          http.response(200)
          |> set_resp_json(json.object([
            tuple("preview", glance.preview_to_json(preview)),
          ]))
        }
      }
    _, _ ->
      http.response(404)
      |> http.set_resp_body(bit_builder.from_bit_string(<<>>))
  }
  |> http.prepend_resp_header("access-control-allow-origin", "*")
  |> http.prepend_resp_header("access-control-allow-credentials", "true")
  |> http.prepend_resp_header(
    "access-control-allow-headers",
    "content-type, sentry-trace",
  )
}
