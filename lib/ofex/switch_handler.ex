require Logger

defmodule Ofex.SwitchHandler do
  use GenServer
  alias OfProto.Messages.Hello

  @port 6633

  def start_link(name) do
    GenServer.start_link(__MODULE__, :ok, name: name)
  end

  def handle_cast({:new_switch, socket}, {}) do
    Logger.info("New switch!")

    :gen_tcp.send(socket, Hello.encode(%Hello{}))

    spawn_link(fn -> recv_loop(socket) end)
    {:noreply, {}}
  end

  def accept(port) do
    {:ok, socket} = :gen_tcp.listen(port,[:binary, packet: :raw,
                                                   active: false,
                                                   reuseaddr: true,
                                                   nodelay: true])
      Logger.info "Accepting connections on port #{port}"

      spawn_link(fn -> accept_loop(socket) end)
      {:ok, {}}
  end

  def init(:ok) do
    accept(@port)
  end

  def accept_loop(socket) do
    {:ok, client} = :gen_tcp.accept(socket)
    GenServer.cast(__MODULE__, {:new_switch, client})
    accept_loop(socket)
  end

  def recv_loop(socket) do
    msg = recv_message(socket)
    recv_loop(socket)
  end

  def recv_message(socket) do
    {:ok, data } = :gen_tcp.recv(socket, 0)
    IO.inspect data
  end
end
