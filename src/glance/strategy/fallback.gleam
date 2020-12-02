import gleam/list
import gleam/option.{Some}
import gleam/result.{unwrap}
import gleam/uri.{Uri}
import gleam/http.{Response}
import gleam/httpc
import floki
import glance/preview.{Page}

pub fn scan(uri) {
  let Uri(path: path, host: Some(host), ..) = uri
  let request =
    http.default_req()
    |> http.set_method(http.Get)
    |> http.set_host(host)
    |> http.set_path(path)
  assert Ok(Response(status: 200, body: html, ..)) = httpc.send(request)
  assert Ok(document) = floki.parse_document(html)

  Page(
    title: unwrap(get_title(document), host),
    description: get_description(document),
    image: get_image(document),
    url: unwrap(get_url(document), uri.to_string(uri)),
  )
}

fn lazy_or(result, fallback) {
  case result {
    Ok(value) -> Ok(value)
    Error(_) -> fallback()
  }
}

// try is the best way of doing result based or, no works wrong way round
fn get_title(document) {
  get_og_title(document)
  |> lazy_or(fn() { get_twitter_title(document) })
  |> lazy_or(fn() { get_document_title(document) })
  |> lazy_or(fn() { get_header_tag_title(document) })
}

pub fn get_og_title(document) {
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

fn get_url(document) {
  get_og_url(document)
  |> lazy_or(fn() { get_canonical_url(document) })
}

fn get_og_url(document) {
  let og_tags = floki.find(document, "meta[property='og:url']")
  list.head(floki.attribute(og_tags, "content"))
}

fn get_canonical_url(document) {
  let canonical_link_tags = floki.find(document, "link[rel=canonical]")
  list.head(floki.attribute(canonical_link_tags, "href"))
}

fn get_description(document) {
  case get_og_description(document) {
    Ok(description) -> description
    Error(Nil) ->
      case get_twitter_description(document) {
        Ok(description) -> description
        Error(Nil) ->
          case get_document_description(document) {
            Ok(description) -> description
            Error(Nil) -> "TODO fallback description"
          }
      }
  }
}

fn get_og_description(document) {
  let og_tags = floki.find(document, "meta[property='og:description']")
  list.head(floki.attribute(og_tags, "content"))
}

fn get_twitter_description(document) {
  let twitter_tags =
    floki.find(document, "meta[property='twitter:description']")
  list.head(floki.attribute(twitter_tags, "content"))
}

fn get_document_description(document) {
  let description_tags = floki.find(document, "meta[name='description']")
  case list.head(description_tags) {
    Ok(description_tag) -> Ok(floki.text([description_tag]))
    Error(Nil) -> Error(Nil)
  }
}

fn get_image(document) {
  case get_og_image(document) {
    Ok(image) -> image
    Error(Nil) -> "TODO fallback image"
  }
}

fn get_og_image(document) {
  let og_tags = floki.find(document, "meta[property='og:image']")
  list.head(floki.attribute(og_tags, "content"))
}
