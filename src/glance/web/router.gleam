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
import open_telemetry/http as otel_http
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

pub fn handle(
  request: Request(BitString),
  tracer: open_telemetry.Tracer,
) -> Response(BitBuilder) {
  open_telemetry.with_span(
    tracer,
    "HTTP.SERVER",
    fn(span_context) {
      open_telemetry.set_attributes(
        span_context,
        otel_http.request_attributes(request),
      )
      open_telemetry.set_attribute(span_context, "lightstep.access_token", "YftAtQbL7Y3jdENkbqa2CtbhJPSEv1E6VRvnpTseszE2ZPOWFzNpNk9rLpZakyPZl6ljpH3WHuWcF3o7aPCvmLIa8eVN36G/RhYgx3jz>")
      let response =
        case request.method {
          http.Options ->
            http.response(200)
            |> http.set_resp_body(bit_builder.from_bit_string(<<>>))
          _ ->
            case request.query {
              Some(target) -> {
                assert Ok(uri.Uri(path: path, host: Some(host), ..)) =
                  uri.parse(target)
                  // Run the span on the client call
                assert Ok(Response(status: 200, body: html, ..)) = open_telemetry.with_span(tracer, "HTTP.CLIENT", fn(span_context) {
                    let req =
                    http.default_req()
                    |> http.set_method(http.Get)
                    |> http.set_host(host)
                    |> http.set_path(path)
                    httpc.send(req)
                })
                  // Run the span on the parsing step
                let snapshot = open_telemetry.with_span(tracer, "PARSE SNAPSHOT", fn(span_context) {
                    assert Ok(document) = floki.parse_document(html)
                    strategy.apply(host, document)
                })
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
      open_telemetry.set_attributes(
        span_context,
        otel_http.response_attributes(response),
      )
      response
    },
  )
}
