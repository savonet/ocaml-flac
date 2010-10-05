
exception Internal

exception Not_flac

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

  type error = 
    [
        `Lost_sync
      | `Bad_header
      | `Frame_crc_mismatch
      | `Unparseable_stream
      | `Unknown
    ]

  exception Error of error

  let () =
    Callback.register_exception "flac_dec_exn_error" (Error `Unknown)

  exception Lost_sync
  exception Bad_header
  exception Frame_crc_mismatch
  exception Unparseable_stream

  let mk_proper_exn e = 
    match e with
      | `Lost_sync -> Lost_sync
      | `Bad_header -> Bad_header
      | `Frame_crc_mismatch -> Frame_crc_mismatch 
      | `Unparseable_stream -> Unparseable_stream
      | `Unknown -> Internal

  let raise_proper f = 
    (fun x -> 
       try
         f x
       with
         | Error e -> raise (mk_proper_exn e))

  type info = 
    { 
      sample_rate : int;
      channels : int;
      bits_per_sample : int;
      total_samples : int64;
      md5sum : string
    }

  let mk_option f =
   (fun x ->
      try
        Some (f x)
      with
        | Internal -> None)

  external info : t -> info = "ocaml_flac_decoder_info"

  let info = mk_option info

  let info x = 
    match info x with
      | Some x -> x
      | None -> raise Not_flac

  let info = raise_proper info

  external comments : t -> string * (string array) = "ocaml_flac_decoder_comments" 

  let split_comment comment =
    try
      let equal_pos =
        String.index_from comment 0 '='
      in
      let c1 =
        String.uppercase (String.sub comment 0 equal_pos)
      in
      let c2 =
        String.sub comment (equal_pos + 1) ((String.length comment) - equal_pos - 1)
      in
        c1, c2;
    with Not_found -> comment, ""

  let comments dec =
    let vd, cmts = comments dec in
    vd, (Array.to_list (Array.map split_comment cmts))

  let comments = mk_option comments

  let comments = raise_proper comments

  external state : t -> state = "ocaml_flac_decoder_state"

  let state = raise_proper state

  external create : read_f -> t = "ocaml_flac_decoder_create"

  let create x =
    let dec = create x in
    dec,info dec

  let create = raise_proper create

  external read : t -> float array array = "ocaml_flac_decoder_read"

  let read = raise_proper read

  external read_pcm : t -> string = "ocaml_flac_decoder_read_pcm"

  let read_pcm = raise_proper read_pcm

end

