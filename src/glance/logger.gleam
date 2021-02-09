import gleam/atom
import gleam/base
import gleam/bit_string
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/string
import gleam/beam.{ExitReason, Stacktrace}
import gleam_sentry as sentry
import glance/config.{Config}

pub fn handle(config: Config, reason: ExitReason, stacktrace: Stacktrace, timestamp) {
  assert Ok(response) =
    sentry.capture_exception(config.sentry_client, reason, stacktrace, timestamp)
  response.headers
  |> io.debug
  response.status
  |> io.debug
  Nil
}
// https://github.com/erlang/otp/blob/master/lib/stdlib/src/proc_lib.erl#L804
// this is a proc lib report with two elements
