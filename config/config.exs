use Mix.Config

config :opentelemetry, :processors,
  otel_batch_processor: %{
    exporter:
      {:opentelemetry_zipkin,
       %{
         address: "https://ingest.lightstep.com:443/api/v2/spans",
         local_endpoint: %{service_name: "glance"}
       }}
  }
