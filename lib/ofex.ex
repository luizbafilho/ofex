require Logger

defmodule Ofex do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false
    Logger.info("Controller Started!")

    {:ok ,_} = :ranch.start_listener(:ranch_acceptor, 100, :ranch_tcp, [{:port, 6633}], Ofex.SwitchHandler, [])

    opts = [strategy: :one_for_one, name: Ofex.Supervisor]
    Supervisor.start_link([], opts)
  end
end
