
module Decoder : 
sig
  type t

  val check_packet : Ogg.Stream.packet -> bool

  val create : Ogg.Stream.packet -> Ogg.Stream.t -> t

  val init : t -> Flac.Decoder.t * Flac.Decoder.info

end
