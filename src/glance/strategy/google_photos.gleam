import gleam/list
import gleam/string
import floki
import glance/strategy/default
import glance/snapshot.{ImageReel}

pub fn apply(document) {
  let image_tags =
    floki.find(document, "body img[src*='https://lh3.googleusercontent.com']")
    // The first image is always a profile image
    |> list.drop(1)

  let image_sources = floki.attribute(image_tags, "src")
  |> list.map(fn(src) {
      // The images are all cropped to a tumbnail
      assert [uncropped, _] = string.split(src, "=")
      uncropped
  })
  assert Ok(title) = default.get_og_title(document)
  ImageReel(title: title, images: image_sources)
}
