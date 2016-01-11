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
    IO.puts("New switch!")

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
      IO.puts "Accepting connections on port #{port}"

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
    :gen_tcp.recv(socket, 8) |> process_header(socket) |> process_data(socket)
  end

  def process_header({:ok, header}, socket) do
    <<_, _, length::size(16), _::bitstring>> = header
    body = case length - 8 do
      0 ->
        <<>>
      r ->
        {:ok, data } = :gen_tcp.recv(socket, r)
        data
    end
    header <> body
  end
  def process_header({:error, reason}, socket) do
    IO.puts("[MESSAGE] Message error:")
    IO.inspect(reason)
  end

  def process_data(data, socket) do
    msg = OfProto.decode(data)
    GenServer.cast(__MODULE__, {:new_message, socket, msg})
  end

  defp send_message(socket, msg) do
    socket |> :gen_tcp.send(OfProto.encode(msg))
  end
end
