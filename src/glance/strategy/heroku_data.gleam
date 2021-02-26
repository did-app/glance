import gleam/bit_string
import gleam/dynamic.{Dynamic}
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/string
import gleam/uri.{Uri}
import gleam/http.{Response}
import gleam/httpc
import gleam/json.{Json}
import glance/preview.{Table}

pub fn scan(uri: Uri) {
  case uri.path_segments(uri.path) {
    ["dataclips", clip_id] ->
      case string.split(clip_id, ".") {
        [clip_id] | [clip_id, "csv"] | [clip_id, "json"] -> {
          let Uri(path: path, host: Some(host), ..) = uri
          let request =
            http.default_req()
            |> http.set_method(http.Get)
            |> http.set_host(host)
            |> http.set_path(string.concat(["/dataclips/", clip_id, ".json"]))
            |> http.prepend_req_header("accept-encoding", "identity")
            // Certain services return 403 without user-agent, 
            // e.g. https://plugable.com/ and https://www.diymachines.co.uk 
            |> http.prepend_req_header("user-agent", "glance/0.1.0")
            |> http.set_req_body(<<>>)
          assert Ok(response) = httpc.send_bits(request)
          case response {
            Response(status: 200, body: body, ..) -> {
              assert Ok(body) = bit_string.to_string(body)
              assert Ok(data) = json.decode(body)
              let data = dynamic.from(data)
              assert Ok(title) = dynamic.field(data, "title")
              assert Ok(title) = dynamic.string(title)
              assert Ok(rows) = dynamic.field(data, "values")
              assert Ok(rows) =
                dynamic.typed_list(
                  rows,
                  dynamic.typed_list(
                    _,
                    fn(x: Dynamic) -> Result(Json, String) {
                      Ok(dynamic.unsafe_coerce(x))
                    },
                  ),
                )
              // could be named columns
              assert Ok(fields) = dynamic.field(data, "fields")
              assert Ok(fields) = dynamic.typed_list(fields, dynamic.string)
              Table(title, fields, rows)
            }
          }
        }
      }
  }
}
