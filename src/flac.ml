
type t

type read_status = 
    Continue
  | Abort
  | End_of_stream
  
exception Read of read_status

type read_f = int -> string*int
type seek_f = unit
type tell_f = unit

external create : read_f -> seek_f -> tell_f -> t = "ocaml_flac_decoder_create"

external read : t-> float array array = "ocaml_flac_decoder_read"

external read_pcm : t -> string = "ocaml_flac_decoder_read_pcm"

