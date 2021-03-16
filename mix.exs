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
      {:gleam_stdlib, "~> 0.14.0", override: true},
      # {:gleam_beam, "~> 0.1.0"},
      {:gleam_beam, github: "midas-framework/beam", override: true},
      {:gleam_cowboy, "~> 0.2.2"},
      {:gleam_crypto, "~> 0.2"},
      {:gleam_http, "~> 2.0", override: true},
      # gleam_sentry is hard coded to httpc 1.0.0 shouldn't be
      {:gleam_httpc, "~> 1.0.1", override: true},
      {:gleam_json, "~> 0.1.0"},
      # {:gleam_sentry, "~> 0.1.1"},
      {:gleam_sentry, github: "midas-framework/gleam_sentry", override: true},

      {:floki,
       github: "midas-framework/floki",
       tag: "4bae91f3129fbf517aae084695db5671eb115931",
       manager: :mix,
       override: true}
    ]
  end
end
