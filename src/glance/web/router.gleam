import gleam/atom
import gleam/bit_builder.{BitBuilder}
import gleam/bit_string.{BitString}
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/uri
import gleam/http.{Request, Response}
import gleam/httpc
import gleam/json
import floki
import open_telemetry
import glance/strategy/strategy
import glance/snapshot

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
  let tracer = open_telemetry.get_tracer(atom.create_from_string("glance"))
  open_telemetry.with_span(
    tracer,
    atom.create_from_string("http.request"),
    fn(x) {
      case request.method {
        http.Options ->
          http.response(200)
          |> http.set_resp_body(bit_builder.from_bit_string(<<>>))
        _ ->
          case request.query {
            Some(target) -> {
              assert Ok(uri.Uri(path: path, host: Some(host), ..)) =
                uri.parse(target)
              let req =
                http.default_req()
                |> http.set_method(http.Get)
                |> http.set_host(host)
                |> http.set_path(path)
              assert Ok(Response(status: 200, body: html, ..)) = httpc.send(req)
              assert Ok(document) = floki.parse_document(html)
              let snapshot = strategy.apply(host, document)
              http.response(200)
              |> set_resp_json(json.object([
                tuple("snapshot", snapshot.to_json(snapshot)),
              ]))
            }
          }
      }
      |> http.prepend_resp_header("access-control-allow-origin", "*")
      |> http.prepend_resp_header("access-control-allow-credentials", "true")
      |> http.prepend_resp_header(
        "access-control-allow-headers",
        "content-type",
      )
    },
  )
}
