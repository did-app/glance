defmodule Glance.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    port = port()

    children = [
      %{
        id: :cowboy,
        start: {:gleam@http@cowboy, :start, [&:glance@web@router.handle(&1, nil), port]}
      }
    ]

    :gleam@beam@logger.add_handler(&:glance@logger.handle/3)
    |> IO.inspect
    
    opts = [strategy: :one_for_one, name: PlumMail.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp port() do
    with raw when is_binary(raw) <- System.get_env("PORT"), {port, ""} = Integer.parse(raw) do
      port
    else
      _ -> throw(ArgumentError)
    end
  end
end
