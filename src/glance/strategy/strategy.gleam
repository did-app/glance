import glance/strategy/default

pub fn apply(host, document) {
  case host {
    _ -> default.apply(document)
  }
}
