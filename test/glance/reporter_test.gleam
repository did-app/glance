import gleam/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/io
import gleam/map.{Map}
import gleam/logger.{Level}
import gleam/should

pub type ErlResult {
  Ok
  Error(Dynamic)
}

external fn add_handler(Atom, Atom, Map(Nil, Nil)) -> ErlResult =
  "logger" "add_handler"

external type Pid

external fn spawn(fn() -> Nil) -> Pid =
  "erlang" "spawn"

external fn receive(Int) -> a =
  "process_native" "do_receive"

external fn erl_log(Level, String, List(Nil)) -> ErlResult =
  "logger" "log"

pub fn assert_failure_test() {
  let self = atom.create_from_string("gleam@logger")
  assert Ok = add_handler(self, self, map.from_list([]))
  erl_log(logger.Critical, "blob", [])
  spawn(fn() {
    io.debug("spawned")
    assert Ok = Error(dynamic.from(Nil))
    Nil
  })
  let x = receive(500)
  io.debug(x)
  todo("finish test")
}
