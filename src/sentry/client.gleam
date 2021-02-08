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
pub type Exception {
  Exception(sentry_type: String)
}

pub fn store_event() {
  todo("store event")
}
