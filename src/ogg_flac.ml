

module Decoder = 
struct
  type t = Flac.Decoder.t

  external check_packet : Ogg.Stream.packet -> bool = "ocaml_flac_decoder_check_ogg"

  external create : Ogg.Stream.packet -> Ogg.Stream.t -> t = "ocaml_flac_decoder_ogg_create"

  external info : t -> Flac.Decoder.info = "ocaml_flac_decoder_info"

  external init : t -> unit = "ocaml_flac_decoder_ogg_init"

  let init dec = 
    init dec ;
    dec,info dec 

end

