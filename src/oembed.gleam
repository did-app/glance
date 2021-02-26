// Handle defined https://oembed.com/
// Providers listed here https://oembed.com/providers.json
import gleam/dynamic
import gleam/io
import gleam/option.{Option}
import gleam/result
import gleam/uri.{Uri}
import gleam/http
import gleam/httpc
import gleam/json

pub fn match(uri) {
  let Uri(host: host, path: path, ..) = uri
  try host = option.to_result(host, Nil)
  let segments = uri.path_segments(path)

  // match against schemes
  try provider = case host, segments {
    "twitter.com", [_, "status", _] ->
      Ok(tuple("publish.twitter.com", "/oembed"))
    "mobile.twitter.com", [_, "status", _] ->
      Ok(tuple("publish.twitter.com", "/oembed"))
    "www.twitter.com", [_, "status", _] ->
      Ok(tuple("publish.twitter.com", "/oembed"))
    "reddit.com", ["r", _, "comments", _, _] ->
      Ok(tuple("www.reddit.com", "/oembed"))
    "www.reddit.com", ["r", _, "comments", _, _] ->
      Ok(tuple("www.reddit.com", "/oembed"))

    _, _ -> Error(Nil)
  }

  provider
  |> Ok
}

// discover
pub fn fetch(endpoint, target) {
  let tuple(host, path) = endpoint
  let request =
    http.default_req()
    |> http.set_host(host)
    |> http.set_path(path)
    |> http.set_query([tuple("url", uri.to_string(target))])
    |> http.set_req_body("")
  try response = httpc.send(request)
  io.debug(response)
  try raw = json.decode(response.body)
  decode_resource(dynamic.from(raw))
  |> result.map_error(dynamic.from)
}

pub type Resouce {
  Photo
  Video
  Link
  // Rich(html: String, width: Option(Int), height: Option(Int))
  Rich(html: String)
}

fn decode_resource(raw) {
  try resource_type = dynamic.field(raw, "type")
  try resource_type = dynamic.string(resource_type)
  case resource_type {
    "photo" -> decode_photo_resource(raw)
    "video" -> decode_video_resource(raw)
    "link" -> decode_link_resource(raw)
    "rich" -> decode_rich_resource(raw)
  }
}

fn decode_photo_resource(raw) {
  todo("photo")
}

fn decode_video_resource(raw) {
  todo("video")
}

fn decode_link_resource(raw) {
  todo("link")
}

fn decode_rich_resource(raw) {
  try html = dynamic.field(raw, "html")
  try html = dynamic.string(html)

  // height and width very much not reliable as fields
  // let width = dynamic.field(raw, "width")
  // try result.then(width, dynamic.option(_, dynamic.int))
  Rich(html)
  |> Ok
}
