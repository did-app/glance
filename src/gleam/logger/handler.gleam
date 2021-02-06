import gleam/atom
import gleam/dynamic.{Dynamic}
import gleam/list
import gleam/map
import gleam/result
import gleam/int
import gleam/io
import gleam/os
import gleam/string
import gleam/http
import gleam/httpc
import gleam/json
import sentry/client as sentry

// https://erlang.org/doc/man/logger.html#HModule:log-2
pub fn log(event, config) {
  let level = dynamic.field(event, atom.create_from_string("level"))
  let meta = dynamic.field(event, atom.create_from_string("meta"))
  let msg = dynamic.field(event, atom.create_from_string("msg"))

  case level, meta, msg {
    Ok(level), Ok(metadata), Ok(message) ->
      handle_log_event(level, message, metadata)
  }
}

fn handle_log_event(level, message, metadata) {
  let report_atom = dynamic.from(atom.create_from_string("report"))
  let message_tag = dynamic.element(message, 0)
  let message_detail = dynamic.element(message, 1)

  // timestamp is always added by logger to metadata
  // Include system_time call as fallback
  let timestamp =
    dynamic.field(metadata, atom.create_from_string("time"))
    |> result.then(dynamic.int)
    |> result.lazy_unwrap(fn() { os.system_time(os.Microsecond) })

  case message_tag, message_detail {
    Ok(t), Ok(message_detail) if t == report_atom ->
      handle_report(message_detail, timestamp)
  }
}

// Error type vs error report.
// make gleam@beam@error
fn handle_report(report, timestamp) {
  // https://github.com/erlang/otp/blob/master/lib/stdlib/src/proc_lib.erl#L804
  // this is a proc lib report with two elements
  case dynamic.field(report, atom.create_from_string("report")) {
    Ok(stuff) -> {
      let Ok([report, linked]) = dynamic.list(stuff)
      let Ok(report) = dynamic.typed_list(report, dynamic.tuple2)
      let report = map.from_list(report)
      let Ok(erl_exception) =
        map.get(report, dynamic.from(atom.create_from_string("error_info")))
      let sentry_exception = handle_erl_exception(erl_exception)
      let event =
        json.object([
          // tuple("id"),
          tuple("timestamp", json.int(timestamp)),
          // TODO switch to production
          tuple("environment", json.string("local")),
          tuple("exception", sentry_exception),
        ])
      io.debug(event)
      let request =
        http.default_req()
        |> http.set_method(http.Post)
        |> http.set_scheme(http.Https)
        // https://e3b301fb356a4e61bebf8edb110af5b3@o351506.ingest.sentry.io/5574979
        |> http.set_host("app.getsentry.com")
        |> http.set_path("/api/store")
        |> http.set_query([
          tuple("sentry_key", "e3b301fb356a4e61bebf8edb110af5b3"),
        ])
        |> http.prepend_req_header("content-type", "application/json")
        |> http.set_req_body(json.encode(event))
        |> http.prepend_req_header("content-encoding", "identity")
        |> io.debug()
        |> httpc.send()
        |> io.debug
      Nil
    }
    Error(_) -> {
      io.debug("nope")
      Nil
    }
  }
}

fn handle_erl_exception(exception) {
  let kind =
    dynamic.element(exception, 0)
    |> result.then(dynamic.atom)
  let reason = dynamic.element(exception, 1)
  let stacktrace =
    dynamic.element(exception, 2)
    |> result.then(dynamic.list)
  let error_atom = atom.create_from_string("error")
  case kind, reason, stacktrace {
    Ok(k), Ok(reason), Ok(stacktrace) if k == error_atom ->
      handle_error(reason, stacktrace)
  }
}

fn handle_error(reason, stacktrace) {
  let sentry_type = case dynamic.element(reason, 0) {
    Ok(key) -> sentry.format(key)
    _ -> sentry.format(reason)
  }
  let value =
    dynamic.element(reason, 1)
    |> result.unwrap(dynamic.from(Nil))
    |> sentry.format
  let stacktrace = list.map(stacktrace, frame_from_dynamic)

  // https://develop.sentry.dev/sdk/event-payloads/exception/
  json.object([
    tuple("type", json.string(sentry_type)),
    tuple("value", json.string(value)),
    // tuple("module", "TODO first frame in stacktrace")
    tuple("stacktrace", json.list(stacktrace)),
  ])
}

fn frame_from_dynamic(frame) {
  let module =
    dynamic.element(frame, 0)
    |> result.map(sentry.format)
  let function =
    dynamic.element(frame, 1)
    |> result.map(sentry.format)
  let arity =
    dynamic.element(frame, 2)
    |> result.then(fn(term) {
      case dynamic.int(term) {
        Ok(arity) -> Ok(arity)
      }
    })
  assert Ok(location) =
    dynamic.element(frame, 3)
    |> result.then(dynamic.typed_list(_, dynamic.tuple2))
    |> result.map(map.from_list)

  let filename =
    map.get(location, dynamic.from(atom.create_from_string("file")))
    |> result.map_error(fn(_: Nil) { "Missing key file" })
    // Returns a charlist
    |> result.map(sentry.format)
  let line_number =
    map.get(location, dynamic.from(atom.create_from_string("line")))
    |> result.map_error(fn(_: Nil) { "Missing key line" })
    |> result.then(dynamic.int)

  case module, function, arity, filename, line_number {
    Ok(module), Ok(function), Ok(arity), Ok(filename), Ok(line_number) -> {
      let function = string.join([function, int.to_string(arity)], "/")
      json.object([
        tuple("filename", json.string(filename)),
        tuple("function", json.string(function)),
        tuple("module", json.string(module)),
        tuple("lineno", json.int(line_number)),
      ])
    }
  }
  // tuple("colno", json.string("doesn't exist"))
  // tuple("abs_path", json.string("TODO"))
}
