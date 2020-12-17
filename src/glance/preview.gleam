import gleam/list
import gleam/json

pub type Preview {
  Page(title: String, description: String, image: String, url: String)
  Image(url: String)
  EmbededVideo(iframe: String)
  ImageReel(title: String, images: List(String), url: String)
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
    ImageReel(title, images, url) ->
      json.object([
        tuple("item", json.string("image_reel")),
        tuple("title", json.string(title)),
        tuple("images", json.list(list.map(images, json.string))),
        tuple("url", json.string(url)),
      ])
  }
}
