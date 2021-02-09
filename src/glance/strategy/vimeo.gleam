import gleam/list
import gleam/option
import gleam/string
import gleam/uri.{Uri}
import glance/preview.{EmbededVideo}

// https://github.com/discourse/onebox/blob/master/lib/onebox/engine/vimeo_onebox.rb
const root = "https://player.vimeo.com/video/"

fn preview(id) {
  EmbededVideo(string.append(root, id))
}

pub fn scan(uri: Uri) {
  case uri.path_segments(uri.path) {
    [id, _something_im_not_sure_about] -> preview(id)
    [id] -> preview(id)
  }
}
