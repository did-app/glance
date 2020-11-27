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

// try is the best way of doing result based or, no works wrong way round
fn get_title(tree) {
  case list.head(floki.attribute(
    floki.find(tree, "meta[property='og:title']"),
    "content",
  )) {
    Ok(title) -> title
    // Note returns empty string even if element not found
    // Use list.head option on tree from find
    // or find_first function
    // move all into a strategy og module
    Error(Nil) -> floki.text(floki.find(tree, "head title"))
  }
}

pub fn handle(request: Request(BitString), config: Nil) -> Response(BitBuilder) {
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
          assert Ok(tree) = floki.parse_document(html)
          let title = get_title(tree)
          let data = json.object([tuple("title", json.string(title))])
          http.response(200)
          |> set_resp_json(data)
        }
      }
  }
  |> http.prepend_resp_header("access-control-allow-origin", "*")
  |> http.prepend_resp_header("access-control-allow-credentials", "true")
  |> http.prepend_resp_header("access-control-allow-headers", "content-type")
}
