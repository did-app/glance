import gleam/list
import gleam/option.{Some}
import gleam/string
import gleam/uri.{Uri}
import glance/strategy/fallback
import glance/preview.{EmbededVideo}

pub fn scan(uri) {
  let Uri(path: path, ..) = uri
  case uri.path_segments(path) {
    ["presentation", "d", "e", key, "pub"] -> {
      let uri =
        Uri(..uri, path: string.concat(["/presentation/d/e/", key, "/embed"]))
      EmbededVideo(uri.to_string(uri))
    }

    _ -> fallback.scan(uri)
  }
}
