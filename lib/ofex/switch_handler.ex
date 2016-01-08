require Logger

defmodule Ofex.SwitchHandler do
  @behaviour :ranch_protocol

  def start_link(ref, socket, transport, opts) do
    Logger.info("Switch Connected!")
    pid = spawn_link(__MODULE__, :init, [ref, socket, transport, opts])
    {:ok, pid}
  end

  def init(ref, socket, transport, _opts = []) do
    :ok = :ranch.accept_ack(ref)
    loop(socket, transport)
  end

  def loop(socket, transport) do
    case transport.recv(socket, 0, :infinity) do
      {:ok, data} ->
        IO.puts "Message from switch"
        IO.inspect(data)
        {_,msg,_} = :of_protocol.decode(data)
        IO.inspect msg
        transport.send(socket, :of_protocol.encode(msg))
      what ->
        IO.inspect what
    end
  end
end
