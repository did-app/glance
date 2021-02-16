import gleam/base
import gleam/bit_string
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import gleam/uri.{Uri}
import gleam/crypto
import glance/preview.{Page}
import glance/strategy/fallback

pub fn scan(uri: Uri) {
  case uri.path_segments(uri.path) {
    ["macros", "s", id, "exec"] -> {
      let hash =
        id
        |> bit_string.from_string()
        |> crypto.hash(crypto.Sha256, _)
        |> base.encode64(False)
      case hash {
        "Bpb574x2f7m+UsdIXE8QqDAl0wqNOFVpH4Gk6wvDazQ" ->
          Page(
            "File uploader",
            "Use this link to upload files securely",
            "https://sendmemo.app/images/integrations/upload.svg",
            uri.to_string(uri),
          )
        _ -> fallback.scan(uri)
      }
    }

    _ -> fallback.scan(uri)
  }
}
