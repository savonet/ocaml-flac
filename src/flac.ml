
type t

type read_f = int -> string*int
type seek_f = unit
type tell_f = unit

(** Possible states of a decoder. *)
type state =
    (** The decoder is ready to search for metadata. *)
  [ `Search_for_metadata
    (** The decoder is ready to or is in the process of reading metadata. *)
  | `Read_metadata
    (** The decoder is ready to or is in the process of searching for the
      * frame sync code. *)
  | `Search_for_frame_sync
    (** The decoder is ready to or is in the process of reading a frame. *)
  | `Read_frame
    (** The decoder has reached the end of the stream. *)
  | `End_of_stream
    (** An error occurred in the underlying Ogg layer. *)
  | `Ogg_error
    (** An error occurred while seeking.  The decoder must be flushed
      * or reset before decoding can continue. *)
  | `Seek_error
    (** The decoder was aborted by the read callback. *)
  | `Aborted ]

exception Internal

external init : unit -> unit = "ocaml_flac_stubs_initialize"

let () = 
  init () ;
  Callback.register_exception "flac_exn_internal" Internal

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

external create : read_f -> seek_f -> tell_f -> t = "ocaml_flac_decoder_create"

external read : t-> float array array = "ocaml_flac_decoder_read"

external read_pcm : t -> string = "ocaml_flac_decoder_read_pcm"

external state : t -> state = "ocaml_flac_decoder_state"

