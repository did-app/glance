import gleam/map
import gleam/os
import gleam_sentry

pub type Config {
  Config(sentry_client: gleam_sentry.Client)
}

pub fn from_env() {
  let env = os.get_env()

  assert Ok(environment) = map.get(env, "ENVIRONMENT")
  assert Ok(sentry_dsn) = map.get(env, "SENTRY_DSN")
  assert Ok(sentry_client) = gleam_sentry.init(sentry_dsn, environment)
  Config(sentry_client)
}
