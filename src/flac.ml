
exception Internal

external init : unit -> unit = "ocaml_flac_stubs_initialize"

let () =
  init () ;
  Callback.register_exception "flac_exn_internal" Internal

module Decoder = 
struct
  type t

  type read_f = int -> string*int

  (** Possible states of a decoder. *)
  type state =
    [ 
        `Search_for_metadata
      | `Read_metadata
      | `Search_for_frame_sync
      | `Read_frame
      | `End_of_stream
      | `Ogg_error
      | `Seek_error
      | `Aborted 
    ]

  type info = 
    { 
      sample_rate : int;
      channels : int;
      bits_per_sample : int;
      total_samples : int64;
      md5sum : string
    }

  external info : t -> info = "ocaml_flac_decoder_info"

  let info x = 
    try 
      Some (info x) 
    with 
      | Internal -> None

  external create : read_f -> t = "ocaml_flac_decoder_create"

  external read : t-> float array array = "ocaml_flac_decoder_read"

  external read_pcm : t -> string = "ocaml_flac_decoder_read_pcm"

  external state : t -> state = "ocaml_flac_decoder_state"

end

