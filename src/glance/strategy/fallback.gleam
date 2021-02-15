import gleam/bit_string
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/result.{unwrap}
import gleam/uri.{Uri}
import gleam/http.{Response}
import gleam/httpc
import floki
import glance/preview.{Image, Page}

// https://www.emergeinteractive.com/insights/detail/the-essentials-of-favicons/
// favicon information
pub fn scan(uri) {
  let Uri(path: path, host: Some(host), ..) = uri
  let request =
    http.default_req()
    |> http.set_method(http.Get)
    |> http.set_host(host)
    |> http.set_path(path)
    |> http.set_req_body(<<>>)
  assert Ok(response) = httpc.send_bits(request)
  io.debug(response.headers)
  case http.get_resp_header(response, "content-type") {
    Ok("image/png") | Ok("image/jpg") | Ok("image/jpeg") | Ok("image/gif") ->
      Image(uri.to_string(uri))
    _ -> {
      assert Ok(html) = bit_string.to_string(response.body)
      assert Ok(document) = floki.parse_document(html)
      let url = uri.to_string(uri)
      io.debug(uri)
      // TODO remove this, dont meddle
      // = case host {
      //   // og:data on google meet doesn't include the path, this is not the correct use of og:url
      //   "meet.google.com" -> uri.to_string(uri)
      //   _ -> unwrap(get_url(document), uri.to_string(uri))
      // }
      Page(
        title: unwrap(get_title(document), host),
        description: get_description(document),
        image: get_image(document, uri),
        url: url,
      )
    }
  }
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
            Error(Nil) -> ""
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

fn get_image(document, base) {
  case get_og_image(document) {
    Ok(image) ->  case uri.parse(image) {
      Ok(relative) -> {
      assert Ok(absolute) =  uri.merge(base, relative) 
      uri.to_string(absolute)
      }
    }
    Error(Nil) -> "TODO fallback image"
  }
}

fn get_og_image(document) {
  let og_tags = floki.find(document, "meta[property='og:image']")
  list.head(floki.attribute(og_tags, "content"))
}
