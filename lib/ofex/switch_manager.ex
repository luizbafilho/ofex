require Logger

defmodule Ofex.SwitchManager do
  alias OfProto.Messages, as: M
  use Reagent.Behaviour

  def start(connection) do
    :gen_server.start __MODULE__, connection, []
  end

  use GenServer

  def init(connection) do
    { :ok, connection }
  end

  def handle_info({ Reagent, :ack }, connection) do
    Logger.info("New Switch!")

    connection |> Socket.active!
    connection |> Socket.Stream.send! OfProto.encode(%M.Hello{})

    { :noreply, connection }
  end

  def handle_info({ :tcp, _, data }, connection) do

    data |> OfProto.decode |> process_msg(connection)

    { :noreply, connection }
  end

  def handle_info({ :tcp_closed, _ }, _connection) do
    { :stop, :normal, _connection }
  end

  def process_msg(%M.EchoRequest{xid: xid, data: data}, conn), do: send_message(%M.EchoReply{xid: xid, data: data}, conn)
  def process_msg(%M.Hello{}, conn), do: send_message(%M.FeaturesRequest{}, conn)
  def process_msg(%M.FeaturesReply{datapath_id: dpi}, conn) do
    Ofex.Switches.put(dpi, %Ofex.Switch{datapath_id: dpi})

    Logger.info("Features Reply")
    Logger.info("Datpath id: #{dpi}")

    send_message(table_miss_flow_mod, conn)
  end
  def process_msg(any, _) do
    IO.inspect(any)
  end

  def send_message(msg, conn) do
    conn |> Socket.Stream.send!(OfProto.encode(msg))
  end

  def table_miss_flow_mod do
    match       = %M.Match{}
    action      = %M.Actions.Output{}
    instruction = %M.Instructions.ApplyActions{actions: [action]}

    %M.FlowMod{match: match, instructions: instruction}
  end
end
