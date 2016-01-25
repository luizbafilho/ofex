require Logger

defmodule Ofex.Handler do
  use GenServer

  def start(connection) do
    :gen_server.start __MODULE__, connection, []
  end

  def init(connection), do: { :ok, connection }


  def send_message(msg, conn) do

  end
end
