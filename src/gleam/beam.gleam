import gleam/atom
import gleam/dynamic.{Dynamic}
import gleam/int
import gleam/list
import gleam/map
import gleam/result
import gleam/string
import gleam/beam/charlist.{Charlist}

external fn erl_format(String, List(a)) -> Charlist =
  "io_lib" "format"

pub fn format(term) {
  charlist.to_string(erl_format("~p", [term]))
}

// These are the reasons an erlang process might exit. 
// http://erlang.org/documentation/doc-9.3/doc/reference_manual/errors.html#exit_reasons
// Because of the erlang:exit and erlang:error functions and exit reason of any term is possible
pub type ExitReason {
  Badarg
  Badarith
  Badmatch(Dynamic)
  FunctionClause
  CaseClause(Dynamic)
  IfClause
  TryClause(Dynamic)
  Undef
  Badfun(Dynamic)
  Badarity(Dynamic)
  TimeoutValue
  Noproc
  Nocatch(Dynamic)
  SystemLimit
}

pub fn cast_exit_reason(raw) {
  let badarg = dynamic.from(atom.create_from_string("badarg"))
  let badarith = dynamic.from(atom.create_from_string("badarith"))
  let badmatch = dynamic.from(atom.create_from_string("badmatch"))
  let functionclause = dynamic.from(atom.create_from_string("functionclause"))
  let caseclause = dynamic.from(atom.create_from_string("caseclause"))
  let ifclause = dynamic.from(atom.create_from_string("ifclause"))
  let tryclause = dynamic.from(atom.create_from_string("tryclause"))
  let undef = dynamic.from(atom.create_from_string("undef"))
  let badfun = dynamic.from(atom.create_from_string("badfun"))
  let badarity = dynamic.from(atom.create_from_string("badarity"))
  let timeoutvalue = dynamic.from(atom.create_from_string("timeoutvalue"))
  let noproc = dynamic.from(atom.create_from_string("noproc"))
  let nocatch = dynamic.from(atom.create_from_string("nocatch"))
  let systemlimit = dynamic.from(atom.create_from_string("systemlimit"))

  let key =
    dynamic.element(raw, 0)
    |> result.unwrap(raw)
  case key, dynamic.element(raw, 1) {
    k, Error(_) if k == badarg -> Ok(Badarg)
    k, Error(_) if k == badarith -> Ok(Badarith)
    k, Ok(term) if k == badmatch -> Ok(Badmatch(term))
    k, Error(_) if k == functionclause -> Ok(FunctionClause)
    k, Ok(term) if k == caseclause -> Ok(CaseClause(term))
    k, Error(_) if k == ifclause -> Ok(IfClause)
    k, Ok(term) if k == tryclause -> Ok(TryClause(term))
    k, Error(_) if k == undef -> Ok(Undef)
    k, Ok(term) if k == badfun -> Ok(Badfun(term))
    k, Ok(term) if k == badarity -> Ok(Badarity(term))
    k, Error(_) if k == timeoutvalue -> Ok(TimeoutValue)
    k, Error(_) if k == noproc -> Ok(Noproc)
    k, Ok(term) if k == nocatch -> Ok(Nocatch(term))
    k, Error(_) if k == systemlimit -> Ok(SystemLimit)
    _, _ -> Error(raw)
  }
}

pub fn cast_stacktrace(raw) {
  try raw_frames = dynamic.list(raw)
  list.try_map(raw_frames, cast_stack_frame)
}

fn cast_stack_frame(frame) {
  let module =
    dynamic.element(frame, 0)
    |> result.map(format)
  let function =
    dynamic.element(frame, 1)
    |> result.map(format)
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
    |> result.map(format)
  let line_number =
    map.get(location, dynamic.from(atom.create_from_string("line")))
    |> result.map_error(fn(_: Nil) { "Missing key line" })
    |> result.then(dynamic.int)

  case module, function, arity, filename, line_number {
    Ok(module), Ok(function), Ok(arity), Ok(filename), Ok(line_number) -> {
      let function = string.join([function, int.to_string(arity)], "/")
      // json.object([
      //   tuple("filename", json.string(filename)),
      //   tuple("function", json.string(function)),
      //   tuple("module", json.string(module)),
      //   tuple("lineno", json.int(line_number)),
      // ])
      todo("output")
    }
  }
  // tuple("colno", json.string("doesn't exist"))
  // tuple("abs_path", json.string("TODO"))
}
