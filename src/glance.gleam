import gleam/dynamic
import gleam/option.{Some}
import gleam/string
import gleam/uri.{Uri}
import gleam/json
import perimeter/scrub.{RejectedInput, Report}
import glance/preview
import glance/strategy/fallback
import glance/strategy/drive_uploader
import glance/strategy/google_docs
import glance/strategy/google_photos
import glance/strategy/google_scripts
import glance/strategy/heroku_data
import glance/strategy/loom
import glance/strategy/vimeo
import glance/strategy/youtube
import glance/strategy/xkcd

pub fn scan_uri(uri) {
  case uri {
    Uri(scheme: scheme, host: Some(host), path: path, query: query, ..) ->
      case host {
        "driveuploader.com" -> drive_uploader.scan(uri)
        "docs.google.com" -> google_docs.scan(uri)
        "photos.app.goo.gl" -> google_photos.scan(uri)
        "script.google.com" -> google_scripts.scan(uri)
        "data.heroku.com" -> heroku_data.scan(uri)
        "xkcd.com" | "m.xkcd.com" | "www.xkcd.com" -> xkcd.scan(uri)
        "www.loom.com" -> loom.scan(uri)
        "www.vimeo.com" | "vimeo.com" -> vimeo.scan(uri)
        "www.youtube.com" | "youtube.com" | "m.youtube.com" -> youtube.scan(uri)
        "youtu.be" -> youtube.scan_short(uri)
        _ -> fallback.scan(uri)
      }
      |> Ok()
    _ -> Error(Report(RejectedInput, "Invalid url", "Must be an absolute url"))
  }
}

pub fn preview_to_json(preview) {
  preview.to_json(preview)
}
