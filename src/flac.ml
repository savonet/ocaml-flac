
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
      | `Memory_allocation_error
      | `Uninitialized 
    ]

  exception Lost_sync
  exception Bad_header
  exception Frame_crc_mismatch
  exception Unparseable_stream

  let () =
    Callback.register_exception "flac_dec_exn_lost_sync" Lost_sync;
    Callback.register_exception "flac_dec_exn_bad_header" Bad_header;
    Callback.register_exception "flac_dec_exn_crc_mismatch" Frame_crc_mismatch;
    Callback.register_exception "flac_dec_exn_unparseable_stream" Unparseable_stream

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

  external state : t -> state = "ocaml_flac_decoder_state"

  external create : read_f -> t = "ocaml_flac_decoder_create"

  let create x =
    let dec = create x in
    dec,info dec

  external read : t -> float array array = "ocaml_flac_decoder_read"

  external read_pcm : t -> string = "ocaml_flac_decoder_read_pcm"

end

