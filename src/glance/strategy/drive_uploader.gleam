import gleam/bit_string
import gleam/option.{Some}
import gleam/string
import gleam/uri.{Uri}
import glance/preview.{EmbededVideo}
import gleam/http.{Response}
import gleam/httpc
import floki
import glance/strategy/fallback
import glance/preview.{Page}

pub fn scan(uri: Uri) {
  let Uri(path: path, host: Some(host), ..) = uri
  let request =
    http.default_req()
    |> http.set_method(http.Get)
    |> http.set_host(host)
    |> http.set_path(path)
    |> http.set_req_body(<<>>)
  assert Ok(response) = httpc.send_bits(request)

  assert Ok(html) = bit_string.to_string(response.body)
  assert Ok(document) = floki.parse_document(html)
  let default_preview = fallback.scan(uri)
  let header_title = fallback.get_header_tag_title(document)
  case header_title, default_preview {
    Ok(upload_title), Page(title, description, image, url) ->
      Page(
        "Drive uploader",
        string.concat(["Use this link to upload files to '", upload_title, "'"]),
        image,
        url,
      )
    _, _ -> default_preview
  }
}
