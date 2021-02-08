# Sentry

### Raven

https://github.com/artemeff/raven-erlang

erlang project, integrates with lager and error_logger.

- Has an empty supervisor, just so it can be started as an app which adds a logger handler.
- Every capture results in a call to sentry. https://github.com/artemeff/raven-erlang/blob/master/src/raven.erl#L46
- Uses httpc and manually calls zlib. https://github.com/artemeff/raven-erlang/blob/master/src/raven.erl#L76-L86
- Has a bunch of hardcoded mappers from error types. https://github.com/artemeff/raven-erlang/blob/master/src/raven_error_logger.erl#L108-L184
  - NOTE: these are old format so not useful to reproduce

### Sparrow

https://github.com/ExpressApp/sparrow/tree/master/lib/sparrow

Elixir project, uses new logger

- Starts a coordinator and task supervisor, is this necessary with the infrastructure that already exists in logger?
- It has a Client behaviour module but only one implementation directly in the library.