import gleam/atom
import gleam/dynamic.{Dynamic}
import gleam/map
import gleam/result
import gleam/io
import sentry/client as sentry

// https://erlang.org/doc/man/logger.html#HModule:log-2
pub fn log(event, config) {
  // io.debug(event)
  let level = dynamic.field(event, atom.create_from_string("level"))
  let meta = dynamic.field(event, atom.create_from_string("meta"))
  let msg = dynamic.field(event, atom.create_from_string("msg"))

  // io.debug(level)
  case level, meta, msg {
    Ok(level), Ok(metadata), Ok(message) ->
      handle_log_event(level, message, metadata)
  }
}

fn handle_log_event(level, message, metadata) {
  let report_atom = dynamic.from(atom.create_from_string("report"))
  let message_tag = dynamic.element(message, 0)
  let message_detail = dynamic.element(message, 1)
  case message_tag, message_detail {
    Ok(t), Ok(message_detail) if t == report_atom ->
      handle_report(message_detail)
  }
}

// Error type vs error report.
// make gleam@beam@error
fn handle_report(report) {
  // https://github.com/erlang/otp/blob/master/lib/stdlib/src/proc_lib.erl#L804
  // this is a proc lib report with two elements
  case dynamic.field(report, atom.create_from_string("report")) {
    Ok(stuff) -> {
      let Ok([report, linked]) = dynamic.list(stuff)
      // io.debug(sentry.format(report))
      // io.debug(stuff)
      // let [x, ..rest] = dynamic.unsafe_coerce(stuff)
      // io.debug("----")
      // io.debug(x)
      // io.debug(rest)
      let Ok(report) = dynamic.typed_list(report, dynamic.tuple2)
      let report = map.from_list(report)
      let Ok(erl_exception) = map.get(report, dynamic.from(atom.create_from_string("error_info")))
      
      handle_erl_exception(erl_exception)
    }
    Error(_) -> {
      io.debug("nope")
      Nil
    }
  }
}

fn handle_erl_exception(exception) {
  let kind = dynamic.element(exception, 0)
  |> result.then(dynamic.atom)
  let reason = dynamic.element(exception, 1)
  let stacktrace = dynamic.element(exception, 2)
  let error_atom = atom.create_from_string("error")
  case kind, reason, stacktrace {
    Ok(k), Ok(reason), Ok(stacktrace) if k == error_atom -> handle_error(reason, stacktrace) 
  }
}

fn handle_error(reason, stacktrace) {
  let sentry_type = case dynamic.element(reason, 0) {
    Ok(key) -> sentry.format(key)
    _ -> sentry.format(reason)
  }
  io.debug(sentry_type)
  Nil
}
