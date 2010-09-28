
type t

type read_status =
    Continue
  | Abort
  | End_of_stream

exception Read of read_status

type read_f = int -> string*int
type seek_f = unit
type tell_f = unit

val create : read_f -> seek_f -> tell_f -> t

val read : t-> float array array

val read_pcm : t -> string

val is_eos : t -> bool

