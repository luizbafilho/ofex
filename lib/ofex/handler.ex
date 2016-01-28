require Logger

defmodule State do
  defstruct switches: [],
            subscriptions: [],
            callback_module: Ofex.DefaultCallback
end

defmodule Ofex.Handler do
  use GenServer

  def start_link do
    GenServer.start_link __MODULE__, %State{}, name: __MODULE__
  end

  def handle_cast({:init, switch}, state) do
    state = %{state| switches: [ switch | state.switches]}

    state.callback_module.init(state)

    {:noreply, state}
  end
  def handle_cast(:terminate, state) do
    {:noreply, state}
  end
  def handle_cast({:message, msg, switch}, state) do
    state.callback_module.handle_message(msg, switch)
    {:noreply, state}
  end

  def handle_call(:switches, _from, state) do
    {:reply, state.switches, state}
  end

  def init_handler(switch),         do: GenServer.cast(__MODULE__, {:init, switch})
  def terminate,                    do: GenServer.cast(__MODULE__, :terminate)
  def handle_message(switch, msg),  do: GenServer.cast(__MODULE__, {:message, msg, switch})
  def get_switches,                 do: GenServer.call(__MODULE__, :switches)
end
