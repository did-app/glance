import gleam/dynamic
import gleam/option.{Some}
import gleam/string
import gleam/uri.{Uri}
import gleam/http.{Response}
import gleam/httpc
import gleam/json
import glance/preview.{Page}

pub fn scan(uri) {
  let Uri(host: Some(host), path: path, ..) = uri
  case uri.path_segments(path) {
    [comic_id] -> {
      let request =
        http.default_req()
        |> http.set_method(http.Get)
        |> http.set_host(host)
        |> http.set_path(string.join(["", comic_id, "info.0.json"], "/"))
      assert Ok(Response(status: 200, body: body, ..)) = httpc.send(request)
      assert Ok(json) = json.decode(body)
      let data = dynamic.from(json)
      assert Ok(title) = dynamic.field(data, "safe_title")
      assert Ok(title) = dynamic.string(title)
      // Note not image
      assert Ok(image) = dynamic.field(data, "img")
      assert Ok(image) = dynamic.string(image)
      // Be nice to hide this but probably doesn't matter
      assert Ok(description) = dynamic.field(data, "alt")
      assert Ok(description) = dynamic.string(description)
      Page(title, description, image, uri.to_string(uri))
    }
  }
}
