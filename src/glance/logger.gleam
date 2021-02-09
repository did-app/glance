import gleam/atom
import gleam/base
import gleam/bit_string
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/string
import gleam/uri
import gleam/beam.{ExitReason, Stacktrace}
import gleam/http
import gleam/httpc
import gleam/json

external fn compress(String) -> String =
  "zlib" "compress"

pub fn handle(reason: ExitReason, stacktrace: Stacktrace, timestamp) {
  let detail = case reason {
    beam.Badarg -> tuple("badarg", "")
    beam.Badarith -> tuple("badarith", "")
    beam.Badmatch(term) -> tuple("badmatch", beam.format(term))
    beam.FunctionClause -> tuple("function_clause", "")
    beam.CaseClause(term) -> tuple("case_clause", beam.format(term))
    beam.IfClause -> tuple("if_clause", "")
    beam.TryClause(term) -> tuple("try_clause", beam.format(term))
    beam.Undef -> tuple("undef", "")
    beam.Badfun(term) -> tuple("badfun", beam.format(term))
    beam.Badarity(term) -> tuple("badarity", beam.format(term))
    beam.TimeoutValue -> tuple("timeout_value", "")
    beam.Noproc -> tuple("noproc", "")
    beam.Nocatch(term) -> tuple("nocatch", beam.format(term))
    beam.SystemLimit -> tuple("system_limit", "")
  }
  let sentry_exception =
    json.object([
      tuple("type", json.string(detail.0)),
      tuple("value", json.string(detail.1)),
      tuple(
        "stacktrace",
        json.object([
          tuple(
            "frames",
            json.list(list.map(
              list.reverse(stacktrace),
              fn(frame) {
                let tuple(module, function, arity, filename, line_number) =
                  frame
                let function =
                  string.join([function, int.to_string(arity)], "/")
                json.object([
                  tuple("filename", json.string(filename)),
                  tuple("function", json.string(function)),
                  tuple("module", json.string(atom.to_string(module))),
                  tuple("lineno", json.int(line_number)),
                ])
              },
            )),
          ),
        ]),
      ),
    ])
  let event =
    json.object([
      // tuple("id"),
      tuple("timestamp", json.int(timestamp)),
      // TODO switch to production
      tuple("environment", json.string("local")),
      tuple("exception", sentry_exception),
    ])
  // https://e3b301fb356a4e61bebf8edb110af5b3@o351506.ingest.sentry.io/5574979
  let dsn =
    "https://e3b301fb356a4e61bebf8edb110af5b3@o351506.ingest.sentry.io/5574979"
  let Ok(uri.Uri(userinfo: Some(public_key), host: Some(host), path: path, ..)) =
    uri.parse(dsn)
  assert Ok(tuple("", project_id)) =
    string.split_once(path, "/")
    |> io.debug()

  io.debug(event)
  let host = host
  let auth_header =
    string.concat([
      "Sentry sentry_version=7, sentry_client=sentry_gleam/1, sentry_timestamp=",
      int.to_string(timestamp),
      ", sentry_key=",
      public_key,
    ])

  // The terminating slash is REQUIRED
  let path = string.concat(["/api/", project_id, "/store/"])
  let body =
    base.encode64(bit_string.from_string(compress(json.encode(event))), False)

  let Ok(response) =
    http.default_req()
    |> http.set_method(http.Post)
    |> http.set_scheme(http.Https)
    // https://e3b301fb356a4e61bebf8edb110af5b3@o351506.ingest.sentry.io/5574979
    |> http.set_host(host)
    |> http.set_path(path)
    |> http.prepend_req_header("content-type", "application/json")
    |> http.prepend_req_header("x-sentry-auth", auth_header)
    |> http.prepend_req_header("user-agent", "sentry_gleam/1")
    |> http.prepend_req_header("accept", "applicaton/json")
    |> http.set_req_body(body)
    |> io.debug()
    |> httpc.send()

  response.headers
  |> io.debug
  response.status
  |> io.debug
  Nil
}
// https://github.com/erlang/otp/blob/master/lib/stdlib/src/proc_lib.erl#L804
// this is a proc lib report with two elements
