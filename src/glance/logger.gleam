import gleam/atom
import gleam/base
import gleam/bit_string
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/string
import gleam/beam.{ExitReason, Stacktrace}
import sentry

pub fn handle(reason: ExitReason, stacktrace: Stacktrace, timestamp) {
  let dsn =
    "https://e3b301fb356a4e61bebf8edb110af5b3@o351506.ingest.sentry.io/5574979"
  assert Ok(client) = sentry.init(dsn, "local")
  assert Ok(response) =
    sentry.capture_exception(client, reason, stacktrace, timestamp)
  response.headers
  |> io.debug
  response.status
  |> io.debug
  Nil
}
// https://github.com/erlang/otp/blob/master/lib/stdlib/src/proc_lib.erl#L804
// this is a proc lib report with two elements
