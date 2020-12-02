import gleam/list
import gleam/option.{Some}
import gleam/string
import gleam/uri.{Uri}
import gleam/http.{Response}
import gleam/httpc
import floki
import glance/strategy/fallback
import glance/preview.{ImageReel}

pub fn scan(uri) {
  let Uri(path: path, host: Some(host), ..) = uri
  let request =
    http.default_req()
    |> http.set_method(http.Get)
    |> http.set_host(host)
    |> http.set_path(path)
  assert Ok(Response(status: 200, body: html, ..)) = httpc.send(request)
  assert Ok(document) = floki.parse_document(html)

  let image_tags =
    floki.find(document, "body img[src*='https://lh3.googleusercontent.com']")
    // The first image is always a profile image
    |> list.drop(1)

  let image_sources =
    floki.attribute(image_tags, "src")
    |> list.map(fn(src) {
      // The images are all cropped to a tumbnail
      assert [uncropped, _] = string.split(src, "=")
      uncropped
    })
  assert Ok(title) = fallback.get_og_title(document)
  ImageReel(title: title, images: image_sources, url: uri.to_string(uri))
}
