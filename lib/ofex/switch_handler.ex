require Logger

defmodule Ofex.SwitchHandler do
  use GenServer
  alias OfProto.Messages
  import OfProto.Constants

  @port 6653

  def start_link(name) do
    GenServer.start_link(__MODULE__, :ok, name: name)
  end

  def handle_cast({:new_switch, socket}, {}) do
    Logger.info("New switch!")

    :gen_tcp.send(socket, OfProto.encode(%Messages.Hello{}))

    spawn_link(fn -> recv_loop(socket) end)
    {:noreply, {}}
  end

  def handle_cast({:new_message, socket, msg}, {}) do
    case msg do
      %Messages.EchoRequest{xid: xid, data: data} ->
        socket |> send_message(%Messages.EchoReply{xid: xid, data: data})
        {:noreply, {}}
      %Messages.Hello{} ->
        socket |> send_message(%Messages.FeaturesRequest{})
        {:noreply, {}}
      %Messages.FeaturesReply{datapath_id: datapath_id} ->
        Logger.info("Features Reply")
        IO.inspect(datapath_id)
        {:noreply, {}}
      other ->
        IO.inspect(other)
    end

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
    :gen_tcp.recv(socket, 0) |> process_data(socket)
  end

  def process_data({:ok, data}, socket) do
    msg = OfProto.decode(data)
    GenServer.cast(__MODULE__, {:new_message, socket, msg})
  end

  def process_data({:error, reason}, _socket) do
    Logger.warn("[MESSAGE] Message error:")
    IO.inspect(reason)
  end

  defp send_message(socket, msg) do
    socket |> :gen_tcp.send(OfProto.encode(msg))
  end
end
