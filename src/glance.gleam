import gleam/dynamic
import gleam/option.{Some}
import gleam/string
import gleam/uri.{Uri}
import gleam/json
import glance/preview
import glance/strategy/fallback
import glance/strategy/drive_uploader
import glance/strategy/google_photos
import glance/strategy/google_scripts
import glance/strategy/loom
import glance/strategy/vimeo
import glance/strategy/youtube
import glance/strategy/xkcd

// return surface or card or widget
// verb scan and verb preview fill same roll
// what about preview.from_url
pub fn scan_uri(uri) {
  let Uri(scheme: scheme, host: Some(host), path: path, query: query, ..) = uri

  case host {
    "driveuploader.com" -> drive_uploader.scan(uri)
    "photos.app.goo.gl" -> google_photos.scan(uri)
    "script.google.com" -> google_scripts.scan(uri)
    "xkcd.com" | "m.xkcd.com" | "www.xkcd.com" -> xkcd.scan(uri)
    "www.loom.com" -> loom.scan(uri)
    "www.vimeo.com" | "vimeo.com" -> vimeo.scan(uri)
    "www.youtube.com" | "youtube.com" | "m.youtube.com" -> youtube.scan(uri)
    "youtu.be" -> youtube.scan_short(uri)
    _ -> fallback.scan(uri)
  }
}

pub fn preview_to_json(preview) {
  preview.to_json(preview)
}
// glance.fetch_resource()
// glance.run
// in client glance/Page
// glance.generate_view
// glance.inspect_item
// study scan resource
// scan item
// item shape is article
// let req =
//   http.default_req()
//   |> http.set_method(http.Get)
//   |> http.set_host(host)
//   |> http.set_path(path)
// // get aspect/preview/facade
// // card: page
// // card: article
// // card: xkcd
// // card: image reel
// // box:
// // payload
// // item: or object
// // resource: card
// // attachment
// // node in the graph
// // shape:
// // summary: always the same keys
// // extended
// assert Ok(Response(status: 200, body: html, ..)) = httpc.send(req)
// assert Ok(document) = floki.parse_document(html)
// let snapshot = strategy.apply(host, document)
