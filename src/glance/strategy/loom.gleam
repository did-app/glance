import gleam/string
import gleam/uri.{Uri}
import glance/preview.{EmbededVideo}

pub fn scan(uri: Uri) {
  case uri.path_segments(uri.path) {
    ["share", id] | ["embed", id] ->
      EmbededVideo(string.append("https://www.loom.com/embed/", id))
  }
}
