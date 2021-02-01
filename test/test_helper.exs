ExUnit.start()

test_files = Path.wildcard(Path.join(__DIR__, "glance/**/*_test.gleam"))

for test_file <- test_files do
  gleam_name =
  test_file
  |> Path.relative_to(__DIR__)
  |> String.replace(".gleam", "")
  |> String.replace("/", "@")
  |> String.to_atom()
  
  elixir_name =
  test_file
  |> Path.relative_to(__DIR__)
  |> String.replace(".gleam", "")
  |> String.replace("/", ".")
  
  tests =
  for {test_name, 0} <- Kernel.apply(gleam_name, :module_info, [:exports]) do
    if String.ends_with?(Atom.to_string(test_name), "_test") do
      {gleam_name, test_name}
    end
  end
  
  tests =
  for {{gleam_name, test_name}, i} <- Enum.with_index(tests) do
    quote line: i + 1 do
      @tag [{unquote(test_name), true}]
      test unquote(test_name) do
        # Kernel.apply(unquote(gleam_name), unquote(test_name), [])
        unquote(gleam_name).unquote(test_name)()
      end
    end
  end
  
  module =
  quote do
    use ExUnit.Case
    unquote(tests)
  end
  
  Module.create(String.to_atom("Elixir." <> elixir_name), module, file: test_file, line: 1)
end
