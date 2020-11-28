import gleam/list
import floki
import glance/snapshot.{OpenGraph}

pub fn apply(document) {
  OpenGraph(
    title: get_title(document),
    og_type: "TODO",
    url: "TODO",
    image: "TODO",
  )
}

// try is the best way of doing result based or, no works wrong way round
fn get_title(document) {
  case get_og_title(document) {
    Ok(title) -> title
    Error(Nil) ->
      case get_twitter_title(document) {
        Ok(title) -> title
        Error(Nil) ->
          case get_document_title(document) {
            Ok(title) -> title
            Error(Nil) ->
              case get_header_tag_title(document) {
                Ok(title) -> title
              }
          }
      }
  }
}

fn get_og_title(document) {
  let og_tags = floki.find(document, "meta[property='og:title']")
  list.head(floki.attribute(og_tags, "content"))
}

fn get_twitter_title(document) {
  let twitter_tags = floki.find(document, "meta[property='twitter:title']")
  list.head(floki.attribute(twitter_tags, "content"))
}

fn get_document_title(document) {
  let title_tags = floki.find(document, "head title")
  case list.head(title_tags) {
    Ok(title_tag) -> Ok(floki.text([title_tag]))
    Error(Nil) -> Error(Nil)
  }
}

fn get_header_tag_title(document) {
  let h1_tags = floki.find(document, "h1")
  case list.head(h1_tags) {
    Ok(header_tag) -> Ok(floki.text([header_tag]))
  }
}
