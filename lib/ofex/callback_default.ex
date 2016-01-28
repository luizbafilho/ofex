require Logger

defmodule Ofex.DefaultCallback do
  alias OfProto.Messages.{PacketIn}

  def init(state) do
    Logger.info("Initializing Handler!")
    IO.inspect state
  end

  def terminate do
    Logger.info("Connection closed!")
  end

  def handle_message(%PacketIn{} = msg, switch) do
    IO.inspect msg
  end

  def handle_message(msg) do
    IO.puts "Unkown MESSAGE"
  end
end
