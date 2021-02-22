import gleam/atom
import gleam/base
import gleam/bit_string
import gleam/dynamic.{Dynamic}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/string
import gleam/beam.{ExitReason, Stacktrace}
import gleam/json
import gleam_sentry as sentry
import glance/config.{Config}

pub fn handle(
  config: Config,
  reason: Dynamic,
  stacktrace: Stacktrace,
  timestamp,
) {
  case beam.cast_exit_reason(reason) {
    Ok(reason) -> {
        sentry.capture_exception(
          config.sentry_client,
          reason,
          stacktrace,
          timestamp,
        )
      Nil
    }
    _ ->
      case beam.cast_dev_reason(reason) {
        Ok(reason) -> {
          let sentry.Client(environment: environment, ..) = config.sentry_client
          let tuple(location, sentry_type, value) = case reason {
            beam.Todo(location, message) -> tuple(location, "todo", message)
            beam.AssertNotMatched(location, value) -> tuple(location, 
              "assert not matched",
              beam.format(value),
            )
          }
          let beam.Location(module, file, line) = location
          let exception =
            json.object([
              tuple("type", json.string(sentry_type)),
              tuple("value", json.string(value)),
              tuple("module", json.string(module)),
              tuple("stacktrace", sentry.stacktrace_to_json(stacktrace)),
            ])
          let event =
            json.object([
              tuple("timestamp", json.int(timestamp)),
              tuple("environment", json.string(environment)),
              tuple("exception", exception),
              tuple("platform", json.string("gleam")),
            ])
            sentry.capture_event(config.sentry_client, event, timestamp)
          Nil
        }
        _ -> Nil
      }
  }
}
// https://github.com/erlang/otp/blob/master/lib/stdlib/src/proc_lib.erl#L804
// this is a proc lib report with two elements
