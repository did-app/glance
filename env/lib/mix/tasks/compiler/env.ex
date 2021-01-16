defmodule Mix.Tasks.Compile.Env do
  use Mix.Task.Compiler

  def run(_args) do
    # case Mix.shell().cmd(
    #        # "gleam compile-package --src /opt/app/provider --name provider --out /opt/app/src/provider"
    #        "gleam build /opt/app/provider"
    #      ) do
    #   0 -> {:ok, []}
    #   status -> exit(status)
    # end
    IO.inspect(:code.all_loaded())

    # root = Application.app_dir(:glance)
    root = System.cwd()

    Path.wildcard(root <> "/src/**/*.env")
    |> Enum.each(fn file ->
      contents =
        File.read!(file)
        |> :gleam@env.provide()

      File.write(String.replace(file, ".env", ".gleam"), contents)
    end)

    :ok
  end
end
