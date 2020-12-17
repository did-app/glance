import gleam/list
import gleam/option
import gleam/string
import gleam/uri.{Uri}
import glance/preview.{EmbededVideo}

const root = "https://www.youtube-nocookie.com/embed/"

fn preview(id) {
  EmbededVideo(string.append(root, id))
}

pub fn scan(uri: Uri) {
  case uri.path_segments(uri.path) {
    ["embed", id] -> preview(id)
    _ -> {
      let query = option.unwrap(uri.query, "")
      assert Ok(params) = uri.parse_query(query)
      assert Ok(id) = list.key_find(params, "v")
      preview(id)
    }
  }
}

pub fn scan_short(uri: Uri) {
  case uri.path_segments(uri.path) {
    [id] -> preview(id)
  }
}
