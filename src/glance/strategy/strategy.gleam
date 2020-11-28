import glance/strategy/default
import glance/strategy/google_photos

pub fn apply(host, document) {
  case host {
    "photos.app.goo.gl" -> google_photos.apply(document)
    _ -> default.apply(document)
  }
}
