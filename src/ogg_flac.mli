
module Decoder : 
sig

  type ogg
 
  val check_packet : Ogg.Stream.packet -> bool

  val get_callbacks : Flac.Decoder.write -> ogg Flac.Decoder.callbacks

  val create : Ogg.Stream.packet -> Ogg.Stream.t -> ogg Flac.Decoder.callbacks -> ogg Flac.Decoder.dec

end

module Encoder : 
sig

  type ogg

  val callbacks : ogg Flac.Encoder.callbacks

  val create : 
     ?comments:(string * string) list ->
     Flac.Encoder.params ->
     Ogg.Stream.t -> 
     ogg Flac.Encoder.t * Ogg.Stream.packet * (Ogg.Stream.packet list)

  val finish : ogg Flac.Encoder.t -> unit

end

module Skeleton :
sig

  (** Generate a vorbis fisbone packet with
    * these parameters, to use in an ogg skeleton.
    * Default value for [start_granule] is [Int64.zero],
    * Default value for [headers] is ["Content-type","audio/x-flac"]
    *
    * See: http://xiph.org/ogg/doc/skeleton.html. *)
  val fisbone :
    ?start_granule:Int64.t ->
    ?headers:(string * string) list ->
    serialno:Nativeint.t -> samplerate:Int64.t -> unit -> Ogg.Stream.packet

end

