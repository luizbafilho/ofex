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
    {:noreply, %{state| switches: [ switch | state.switches]}}
  end
  def handle_cast({:message, message}, state) do
    state.callback_module.handle_message(message)
    {:noreply, state}
  end

  def handle_call(:switches, _from, state) do
    {:reply, state.switches, state}
  end

  def init_handler(switch), do: GenServer.cast(__MODULE__, {:init, switch})
  def get_switches, do: GenServer.call(__MODULE__, :switches)
  def handle_message(msg) do
    GenServer.cast(__MODULE__, {:message, msg})
  end
end
