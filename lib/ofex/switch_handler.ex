require Logger

defmodule Ofex.SwitchHandler do
  use GenServer

  @port 6633

  def start_link(name) do
    GenServer.start_link(__MODULE__, :ok, name: name)
  end

  def handle_cast({:new_switch, socket}, {}) do
    spawn_link(fn -> recv_loop(socket) end)
    Logger.info("New switch!")
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
    # {:ok, task} = Task.start_link(fn -> handle_connection(client) end)
    # :ok = :gen_tcp.controlling_process(client, task)
    GenServer.cast(__MODULE__, {:new_switch, client})
    # :ok = :gen_tcp.controlling_process(socket, pid)
    accept_loop(socket)
  end

  def recv_loop(socket) do
    msg = recv_message(socket)
    recv_loop(socket)
  end

  def recv_message(socket) do
    {:ok, <<version::size(8), type::size(8), length::size(16), xid::size(32)>>} = :gen_tcp.recv(socket, 8)
    body = case length - 8 do
      0 ->
        <<>>
      l ->
        {:ok, p} = :gen_tcp.recv(socket, l)
    end

    Logger.info version
    Logger.info type
    Logger.info length
    :gen_tcp.send(socket, <<version::size(8), type::size(8), length::size(16), xid::size(32)>>)
    # case :gen_tcp.recv(socket, 0, :infinity) do
    #   {:ok, data} ->
    #     IO.puts data
    #   what ->
    #     IO.inspect what
    # end
    # handle_connection(socket)
  end

  # def loop(socket, transport) do
  #   case transport.recv(socket, 0, :infinity) do
  #     {:ok, data} ->
  #       IO.puts "Message from switch"
  #       # hello = %OfProto.Messages.Hello{}
  #       # transport.send(socket, hello |> OfProto.Messages.Hello.encode)
  #       hello = OfProto.Messages.Hello.decode(data)
  #       transport.send(socket, hello |> OfProto.Messages.Hello.encode)
  #       features = %OfProto.Messages.FeaturesRequest{xid: 394239292}
  #       transport.send(socket, features |> OfProto.Messages.FeaturesRequest.encode)
  #       # IO.inspect features
  #       # transport.send(socket, msg |> OfProto.Messages.Hello.encode)
  #     what ->
  #       IO.inspect what
  #   end
  # end
end
