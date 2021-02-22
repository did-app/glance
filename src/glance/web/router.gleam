import gleam/bit_builder.{BitBuilder}
import gleam/bit_string.{BitString}
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/uri
import gleam/http.{Get, Options, Request, Response}
import gleam/httpc
import gleam/json
import floki
import glance.{BadCall}

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
  case route(request, config) {
    Ok(response) -> response
    Error(BadCall(detail)) ->
      http.response(400)
      |> http.set_resp_body(bit_builder.from_bit_string(bit_string.from_string(
        detail,
      )))
  }
  |> http.prepend_resp_header("access-control-allow-origin", "*")
  |> http.prepend_resp_header("access-control-allow-credentials", "true")
  |> http.prepend_resp_header(
    "access-control-allow-headers",
    "content-type, sentry-trace",
  )
}

fn route(request: Request(BitString), config: Nil) {
  case request.method, http.path_segments(request) {
    Options, [] ->
      http.response(200)
      |> http.set_resp_body(bit_builder.from_bit_string(<<>>))
      |> Ok()

    Get, [] -> {
      try target = fetch_query(request)
      try uri =
        uri.parse(target)
        |> result.map_error(fn(_) {
          BadCall("Query parameter must be a valid url")
        })
      try preview = glance.scan_uri(uri)
      http.response(200)
      |> set_resp_json(json.object([
        tuple("preview", glance.preview_to_json(preview)),
      ]))
      |> Ok()
    }

    _, _ ->
      http.response(404)
      |> http.set_resp_body(bit_builder.from_bit_string(<<>>))
      |> Ok()
  }
}

fn fetch_query(request) {
  let Request(query: query, ..) = request
  case query {
    Some(target) -> Ok(target)
    None -> Error(BadCall("Request must have a query parameter"))
  }
}
