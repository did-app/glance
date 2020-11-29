import gleam/atom.{Atom}

pub external type Tracer

pub external type SpanContext

pub external fn get_tracer(name: Atom) -> Tracer =
  "opentelemetry" "get_tracer"

pub external fn with_span(
  tracer: Tracer,
  name: String,
  func: fn(SpanContext) -> t,
) -> t =
  "otel_tracer" "with_span"

pub external fn set_attribute(
  span: SpanContext,
  key: String,
  value: String,
) -> Nil =
  "otel_span" "set_attribute"

pub external fn set_attributes(
  span: SpanContext,
  attributes: List(tuple(String, String)),
) -> Nil =
  "otel_span" "set_attributes"
