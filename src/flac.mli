
exception Internal 
exception Not_flac

module Decoder : 
sig
  type t

  type read_f = int -> string*int

  type info =
    {
      sample_rate : int;
      channels : int;
      bits_per_sample : int;
      total_samples : int64;
      md5sum : string
    }

  (** Possible states of a decoder. *)
  type state =
    [
        (** The decoder is ready to search for metadata. *)
        `Search_for_metadata
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
      | `Aborted 
        (** An error occurred allocating memory.  The decoder is in an invalid
          * state and can no longer be used. *)
      | `Memory_allocation_error
        (** This state is seen in the case of 
          * an uninitialized ogg decoder. *)
      | `Uninitialized ]

  exception Lost_sync
  exception Bad_header
  exception Frame_crc_mismatch
  exception Unparseable_stream

  val create : read_f -> t * info

  val read : t -> float array array

  val read_pcm : t -> string

  val state : t -> state

  val comments : t -> (string * (string * string) list) option

end
