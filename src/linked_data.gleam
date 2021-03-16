import gleam/dynamic
import gleam/io
import gleam/result
import gleam/json as encoded
import perimeter/input/json as input
import floki

pub fn parse(text) {
  assert Ok(raw) = encoded.decode(text)
  let raw = dynamic.from(raw)

  try entity_type =
    input.required(raw, "@type", input.as_string)
    |> result.map_error(fn(_) { Nil })
  case entity_type {
    "Recipe" ->
      parse_recipe(raw)
      |> result.map_error(fn(_) { Nil })

    _ -> Error(Nil)
  }
}

pub fn parse_recipe(raw) {
  try image = input.required(raw, "image", as_image_object)
  try name = input.required(raw, "name", input.as_string)
  try recipe_instructions =
    input.required(raw, "recipeInstructions", input.as_list(_, as_step))
  Ok(tuple(name, image, recipe_instructions))
}

pub fn as_image_object(raw) {
  assert Ok(entity_type) = input.required(raw, "@type", input.as_string)
  case entity_type {
    "ImageObject" -> {
      assert Ok(url) = input.required(raw, "url", input.as_string)
      Ok(url)
    }
  }
}

pub fn as_step(raw) {
  assert Ok(text) = input.required(raw, "text", as_text)
  Ok(text)
}

pub fn as_text(raw) {
  io.debug(raw)
  assert Ok(raw) = input.as_string(raw)
  assert Ok(p) = floki.parse_document(raw)
  Ok(floki.text(p))
}
