// https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html#charlists
// Elixir makes charlist one word
pub external type Charlist

pub external fn to_string(Charlist) -> String =
  "erlang" "list_to_binary"
