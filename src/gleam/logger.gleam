import gleam/io

// These are the log levels used in the erlang logger
// https://erlang.org/doc/apps/kernel/logger_chapter.html#log-level
// They match with a set defined in the Syslog Protocol, RFC 5424
pub type Level {
  Emergency
  Alert
  Critical
  Error
  Warning
  Notice
  Info
  Debug
}

// Erl log event is this or string or format plus args
pub type Event {
  Event(level: Level, meta: Int, message: Int)
}

// This function must be exported by the module to be added as a handler to 
pub fn log(log_event, metadata) {
  io.debug(log_event)
  io.debug(metadata)
}
