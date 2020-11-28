use Mix.Config

# Translated from https://github.com/open-telemetry/opentelemetry-erlang/blob/d33a383f41a4d6c4dee4883ce5eb54e2230a7c74/config/sys.config#L1
# config :opentelemetry, :processors, otel_batch_processor: %{exporter: {:otel_exporter_stdout, []}}
config :opentelemetry, :processors,
  otel_batch_processor: %{
    # exporter: {:opentelemetry_exporter, %{endpoints: [{:http, "localhost", 14250}]}}
    exporter:
      {:opentelemetry_zipkin,
       %{address: "http://localhost:9411/api/v2/spans", local_endpoint: %{service_name: "foo"}}}
  }
