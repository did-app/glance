import gleam/io

pub fn handle(reason, stacktrace, timestamp) {
  io.debug(reason)
  io.debug(stacktrace)
  io.debug(timestamp)
  Nil
}

// Error type vs error report.
// make gleam@beam@error
// fn handle_report(report, timestamp) {
//   // https://github.com/erlang/otp/blob/master/lib/stdlib/src/proc_lib.erl#L804
//   // this is a proc lib report with two elements
//   case dynamic.field(report, atom.create_from_string("report")) {
//     Ok(stuff) -> {
//       let Ok([report, linked]) = dynamic.list(stuff)
//       let Ok(report) = dynamic.typed_list(report, dynamic.tuple2)
//       let report = map.from_list(report)
//       let Ok(erl_exception) =
//         map.get(report, dynamic.from(atom.create_from_string("error_info")))
//       let event =
//         json.object([
//           // tuple("id"),
//           tuple("timestamp", json.int(timestamp)),
//           // TODO switch to production
//           tuple("environment", json.string("local")),
//           tuple("exception", sentry_exception),
//         ])
//       io.debug(event)
//       let request =
//         http.default_req()
//         |> http.set_method(http.Post)
//         |> http.set_scheme(http.Https)
//         // https://e3b301fb356a4e61bebf8edb110af5b3@o351506.ingest.sentry.io/5574979
//         |> http.set_host("app.getsentry.com")
//         |> http.set_path("/api/store")
//         |> http.set_query([
//           tuple("sentry_key", "e3b301fb356a4e61bebf8edb110af5b3"),
//         ])
//         |> http.prepend_req_header("content-type", "application/json")
//         |> http.set_req_body(json.encode(event))
//         |> http.prepend_req_header("content-encoding", "identity")
//         |> io.debug()
//         |> httpc.send()
//         |> io.debug
//       Nil
//     }
//     Error(_) -> {
//       io.debug("nope")
//       Nil
//     }
//   }
// }