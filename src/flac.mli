
exception Internal 
exception Not_flac

module Decoder : 
sig
  type 'a dec

  type 'a t

  type write = float array array -> unit

  type read = int -> string*int

  type 'a callbacks 

  type generic

  type info =
    {
      sample_rate : int;
      channels : int;
      bits_per_sample : int;
      total_samples : int64;
      md5sum : string
    }

  type comments = string * ((string*string) list)

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

  val get_callbacks :
           ?seek:(int64 -> unit) ->
           ?tell:(unit -> int64) ->
           ?length:(unit -> int64) ->
           ?eof:(unit -> bool) ->
           read -> write -> generic callbacks

  val create : 'a callbacks -> 'a dec

  val init : 'a dec -> 'a callbacks -> ('a t) * info * (comments option)

  val process : 'a t -> 'a callbacks -> unit

  val state : 'a t -> 'a callbacks -> state

  val to_s16le : float array array -> string

  module File :
  sig

    type file

    type handle =
     {
       fd : Unix.file_descr ;
       dec : file t ;
       callbacks : file callbacks ;
       info : info ;
       comments : (string * ((string * string) list)) option ;
     }

    val create_from_fd : write -> Unix.file_descr -> handle

    val create : write -> string -> handle
  end

end

module Encoder : 
sig
  type 'a t

  type write = string -> unit

  type 'a callbacks

  type generic

  type params =
    {
      channels : int ;
      bits_per_sample : int ;
      sample_rate : int ;
      compression_level : int option;
      total_samples : int64 option ;
    }

  type comments = (string * string) list

  exception Invalid_data

  val get_callbacks :
       ?seek:(int64 -> unit) ->
       ?tell:(unit -> int64) -> 
       write -> generic callbacks

  val create : ?comments:comments -> params -> 'a callbacks -> 'a t

  val process : 'a t -> 'a callbacks -> float array array -> unit

  val finish : 'a t -> 'a callbacks -> unit

  val from_s16le : string -> int -> float array array

  module File :
  sig

    type file

    type handle =
     {
       fd : Unix.file_descr ;
       enc : file t ;
       callbacks : file callbacks
     }

    val create_from_fd : ?comments:comments -> params -> Unix.file_descr -> handle

    val create : ?comments:comments -> params -> string -> handle

  end
end

