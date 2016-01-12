require Logger

defmodule Ofex do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false
    Logger.info("Controller Started!")

    children = [
      worker(Reagent, [Ofex.SwitchManager, [port: 6653]]),
      worker(Ofex.Switches, [])
    ]

    opts = [strategy: :one_for_one, name: Ofex.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
