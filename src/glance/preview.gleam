import gleam/list
import gleam/option.{Option}
import gleam/json

pub type Preview {
  Page(title: String, description: String, image: String, url: String)
  Image(url: String)
  EmbededVideo(iframe: String)
  EmbededHtml(html: String)
  ImageReel(title: String, images: List(String), url: String)
  // title can be path name if nothing better
  Table(title: String, fields: List(String), rows: List(List(json.Json)))
}

pub fn to_json(preview) {
  case preview {
    Page(title, description, image, url) ->
      json.object([
        tuple("item", json.string("page")),
        tuple("title", json.string(title)),
        tuple("description", json.string(description)),
        tuple("image", json.string(image)),
        tuple("url", json.string(url)),
      ])
    Image(url) ->
      json.object([
        tuple("item", json.string("image")),
        tuple("url", json.string(url)),
      ])
    EmbededVideo(iframe) ->
      json.object([
        tuple("item", json.string("embeded_video")),
        tuple("iframe", json.string(iframe)),
      ])
    EmbededHtml(html) ->
      json.object([
        tuple("item", json.string("embeded_html")),
        tuple("html", json.string(html)),
      ])
    // tuple("width", json.nullable(width, json.int)),
    // tuple("height", json.nullable(height, json.int)),
    ImageReel(title, images, url) ->
      json.object([
        tuple("item", json.string("image_reel")),
        tuple("title", json.string(title)),
        tuple("images", json.list(list.map(images, json.string))),
        tuple("url", json.string(url)),
      ])
    Table(title, fields, rows) ->
      json.object([
        tuple("item", json.string("table")),
        tuple("title", json.string(title)),
        tuple("fields", json.list(list.map(fields, json.string))),
        tuple("rows", json.list(list.map(rows, json.list))),
      ])
  }
}
