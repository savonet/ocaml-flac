(*
 * Copyright 2003-2010 Savonet team
 *
 * This file is part of Ocaml-flac.
 *
 * Ocaml-flac is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-flac is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-flac; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(* Author; Romain Beauxis <toots@rastageeks.org> *)

module Decoder = struct
  external get_packet_data : Ogg.Stream.packet -> string
    = "ocaml_flac_decoder_packet_data"

  let ogg_header_len = 9

  let create ~fill ~write os =
    let read_data = Buffer.create 1024 in
    let is_first_packet = ref true in
    let rec read bytes ofs len =
      try
        if Buffer.length read_data = 0 then (
          let p = Ogg.Stream.get_packet os in
          let data = get_packet_data p in
          let data =
            if !is_first_packet then (
              let len = String.length data in
              assert (len > ogg_header_len);
              String.sub data ogg_header_len (len - ogg_header_len))
            else data
          in
          is_first_packet := false;
          Buffer.add_string read_data data);
        let c = Buffer.contents read_data in
        let c_len = String.length c in
        let len = min len c_len in
        let rem = String.sub c len (c_len - len) in
        Buffer.reset read_data;
        Buffer.add_string read_data rem;
        Bytes.blit_string c 0 bytes ofs len;
        len
      with
        | Ogg.Not_enough_data ->
            fill ();
            read bytes ofs len
        | Ogg.End_of_stream -> 0
    in
    Flac.Decoder.create ~read ~write ()

  external check_packet : Ogg.Stream.packet -> bool
    = "ocaml_flac_decoder_check_ogg"
end

module Encoder = struct
  type priv
  type t = { encoder : Flac.Encoder.t; first_pages : Ogg.Page.t list }

  external create :
    (string * string) array ->
    Flac.Encoder.params ->
    nativeint ->
    (bytes -> unit) ->
    priv = "ocaml_flac_encoder_ogg_create"

  let create ?(comments = []) ~serialno ~write params =
    if params.Flac.Encoder.channels <= 0 then raise Flac.Encoder.Invalid_data;
    let comments = Array.of_list comments in
    let first_pages_parsed = ref false in
    let first_pages = ref [] in
    let header = ref None in
    let write_wrap write p =
      match !header with
        | Some h ->
            header := None;
            write (Bytes.unsafe_to_string h, Bytes.unsafe_to_string p)
        | None -> header := Some p
    in
    let write_first_page p = first_pages := p :: !first_pages in
    let write =
      write_wrap (fun p ->
          if !first_pages_parsed then write p else write_first_page p)
    in
    let enc = create comments params serialno write in
    first_pages_parsed := true;
    assert (!header = None);
    { encoder = Obj.magic (enc, params); first_pages = List.rev !first_pages }
end

module Skeleton = struct
  external fisbone :
    Nativeint.t -> Int64.t -> Int64.t -> string -> Ogg.Stream.packet
    = "ocaml_flac_skeleton_fisbone"

  let fisbone ?(start_granule = Int64.zero)
      ?(headers = [("Content-type", "audio/x-flac")]) ~serialno ~samplerate () =
    let concat s (h, v) = Printf.sprintf "%s%s: %s\r\n" s h v in
    let s = List.fold_left concat "" headers in
    fisbone serialno samplerate start_granule s
end
