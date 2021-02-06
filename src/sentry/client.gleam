// Note sentry has a schema Repo to get event Shapes from https://github.com/getsentry/sentry-data-schemas
// https://develop.sentry.dev/sdk/event-payloads/
pub type Event {
  Event(
    id: String,
    timestamp: String,
    // platform is a required value, but will always be other (Gleam or Beam not options) 
    // level, Using the sentry default of error 
    // logger, The name of the logger which created the record.
    // transaction, The name of the transaction which caused this exception.
    // server_name, Identifies the host from which the event was recorded. foo.example.com
    // release, The release version of the application. git sha is sensible
    // dist, Distributions are used to disambiguate build or deployment variants of the same release of an application.
    // tags, A map or list of tags for this event. Each tag must be less than 200 characters.
    environment: String,
  )
}

// modules
// extra, An arbitrary mapping of additional metadata to store with the event.
// fingerprint, A list of strings used to dictate the deduplication of this event.
// https://docs.sentry.io/data-management/event-grouping/
// errors, A list of errors in capturing or handling this event.
external type CharList

external fn erl_format(String, List(a)) -> CharList =
  "io_lib" "format"

external fn list_to_binary(CharList) -> String =
  "erlang" "list_to_binary"

pub fn format(term) {
  list_to_binary(erl_format("~p", [term]))
}

pub type Exception {
  Exception(sentry_type: String)
}
// {error,
//           {badmatch,2},
//           [{glance@web@router,handle,2,
//                [{file,"gen/src/glance@web@router.erl"},{line,20}]},
//            {gleam@http@cowboy,'-service_to_handler/1-fun-0-',2,
//                [{file,
//                     "/opt/app/deps/gleam_cowboy/gen/src/gleam@http@cowboy.erl"},
//                 {line,60}]},
//            {gleam_cowboy_native,init,2,
//                [{file,
//                     "/opt/app/deps/gleam_cowboy/src/gleam_cowboy_native.erl"},
//                 {line,22}]},
//            {cowboy_handler,execute,2,
//                [{file,"/opt/app/deps/cowboy/src/cowboy_handler.erl"},
//                 {line,37}]},
//            {cowboy_stream_h,execute,3,
//                [{file,"/opt/app/deps/cowboy/src/cowboy_stream_h.erl"},
//                 {line,300}]},
//            {cowboy_stream_h,request_process,3,
//                [{file,"/opt/app/deps/cowboy/src/cowboy_stream_h.erl"},
//                 {line,291}]},
//            {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,226}]}]},
