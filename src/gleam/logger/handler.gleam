import gleam/atom
import gleam/dynamic.{Dynamic}
import gleam/map
import gleam/io


// https://erlang.org/doc/man/logger.html#HModule:log-2
pub fn log(event, config) {
  // io.debug(event)
  let level = dynamic.field(event, atom.create_from_string("level"))
  let meta = dynamic.field(event, atom.create_from_string("meta"))
    let msg = dynamic.field(event, atom.create_from_string("msg"))

  // io.debug(level)
  case level, meta, msg {
    Ok(level), Ok(metadata), Ok(message) -> handle_log_event(level, message, metadata)
  }
}

fn handle_log_event(level, message, metadata) {
  let report_atom = dynamic.from(atom.create_from_string("report"))
  let message_tag = dynamic.element(message, 0)
  let message_detail = dynamic.element(message, 1)
  case message_tag, message_detail {
    Ok(t), Ok(message_detail) if t == report_atom -> handle_report(message_detail)
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
      // io.debug(stuff)
      // let [x, ..rest] = dynamic.unsafe_coerce(stuff)
      // io.debug("----")
      // io.debug(x)
      // io.debug(rest)
      io.debug(map.from_list(dynamic.unsafe_coerce(report)))
      Nil
    }
    Error(_) -> {
      io.debug("nope")
      Nil}
  }
}