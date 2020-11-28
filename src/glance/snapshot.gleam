import gleam/list
import gleam/json

// I think the shape of this shuld not include open graph, instead it should be reel card etc
pub type SnapShot {
  OpenGraph(
    // These are the required OG meta data types
    title: String,
    og_type: String,
    // TODO is it worth parsing these as URL's, check they load
    url: String,
    image: String,
    // optional, but some we always find a value for
    description: String,
  )
  ImageReel(title: String, images: List(String))
}

// OG also defines several optional types depending on the type
pub fn to_json(snapshot) {
  case snapshot {
    OpenGraph(title, og_type, url, image, description) ->
      json.object([
        tuple("snapshot", json.string("card")),
        tuple("title", json.string(title)),
        tuple("type", json.string(og_type)),
        tuple("url", json.string(url)),
        tuple("image", json.string(image)),
        tuple("description", json.string(description)),
      ])
    ImageReel(title, images) ->
      json.object([
        tuple("snapshot", json.string("image_reel")),
        tuple("title", json.string(title)),
        tuple("images", json.list(list.map(images, json.string))),
      ])
  }
}
