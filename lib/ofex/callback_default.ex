defmodule Ofex.DefaultCallback do
  alias OfProto.Messages, as: M

  def handle_message(%M.PacketIn{} = msg) do
    IO.inspect msg
  end

  def handle_message(msg) do
    IO.puts "Unkown MESSAGE"
  end
end
