defmodule PlumMail.MixProject do
  use Mix.Project

  def project do
    [
      app: :glance,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      erlc_paths: ["src", "gen"],
      compilers: [:gleam | Mix.compilers()],
      deps: deps()
    ]
  end

  def application do
    [
      # NOTE gleam_http should be required by cowboy
      extra_applications: [:logger, :gleam_http],
      mod: {Glance.Application, []}
    ]
  end

  defp deps do
    [
      {:mix_gleam, "~> 0.1.0"},
      {:gleam_stdlib, "~> 0.12.0", override: true},
      {:gleam_cowboy, "~> 0.1.2"},
      {:gleam_http, "~> 1.6"},
      {:gleam_httpc, "~> 0.1.1"},
      {:gleam_json, "~> 0.1.0"},
      {:floki,
       github: "midas-framework/floki",
       tag: "4bae91f3129fbf517aae084695db5671eb115931",
       manager: :mix,
       override: true}
    ]
  end
end
