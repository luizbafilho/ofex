require Logger

defmodule Ofex.Driver do
  alias OfProto.Messages, as: M
  use Reagent.Behaviour
  use GenServer

  def start(connection) do
    GenServer.start __MODULE__, %{conn: connection, switch: nil}, []
  end

  def init(state) do
    { :ok, state }
  end

  def handle_cast({:switch_ready, switch}, state) do
    {:noreply, %{state | switch: switch}}
  end

  def handle_info({ Reagent, :ack }, %{conn: conn} = state) do
    Logger.info("New Switch!")

    conn |> Socket.active!
    conn |> Socket.Stream.send! OfProto.encode(%M.Hello{})

    { :noreply, state }
  end

  def handle_info({ :tcp, _, data }, state) do

    data |> OfProto.decode |> process_msg(state)

    { :noreply, state }
  end

  def handle_info({ :tcp_closed, _ }, state) do
    Ofex.Handler.terminate

    { :stop, :normal, state }
  end

  def process_msg(%M.EchoRequest{xid: xid, data: data}, %{conn: conn}), do: send_message(%M.EchoReply{xid: xid, data: data}, conn)
  def process_msg(%M.Hello{}, %{conn: conn}), do: send_message(%M.FeaturesRequest{}, conn)
  def process_msg(%M.FeaturesReply{datapath_id: dpi}, %{conn: conn}) do
    Logger.info("Datpath id: #{dpi}")

    Ofex.Handler.init_handler(%Ofex.Switch{datapath_id: dpi})

    send_message(table_miss_flow_mod, conn)
  end
  def process_msg(msg, %{switch: switch}) do
    Ofex.Handler.handle_message(msg, switch)
  end

  def switch_ready(switch), do: GenServer.cast(__MODULE__, {:switch_ready, switch})

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
