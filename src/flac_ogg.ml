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
  type ogg

  let get_callbacks write : ogg Flac.Decoder.callbacks =
    Obj.magic (Flac.Decoder.get_callbacks (fun _ -> raise Flac.Internal) write)

  external check_packet : Ogg.Stream.packet -> bool
    = "ocaml_flac_decoder_check_ogg"

  external finalize_decoder_private_values : ogg Flac.Decoder.dec -> unit
    = "ocaml_flac_finalize_ogg_decoder_private_values"

  external create :
    Ogg.Stream.packet -> Ogg.Stream.stream -> ogg Flac.Decoder.dec
    = "ocaml_flac_decoder_ogg_create"

  let create p os =
    let dec = create p os in
    Gc.finalise finalize_decoder_private_values dec;
    dec

  external update_ogg_stream : ogg Flac.Decoder.t -> Ogg.Stream.stream -> unit
    = "ocaml_flac_decoder_ogg_update_os"
end

module Encoder = struct
  type ogg
  type enc

  type t = {
    encoder: ogg Flac.Encoder.t;
    callbacks: ogg Flac.Encoder.callbacks;
    first_pages : Ogg.Page.t list
  }

  external finalize_encoder_private_values : enc -> unit
    = "ocaml_flac_finalize_ogg_encoder_private_values"

  external create :
    (string * string) array ->
    Flac.Encoder.params ->
    (Ogg.Page.t -> unit) ->
    nativeint ->
    enc = "ocaml_flac_encoder_ogg_create"

  external set_write_cb : enc -> (Ogg.Page.t -> unit) -> unit = "ocaml_flac_encoder_ogg_set_write_cb"

  let create ?(comments = []) ~serialno params write_cb =
    if params.Flac.Encoder.channels <= 0 then raise Flac.Encoder.Invalid_data;
    let comments = Array.of_list comments in
    let first_pages = Atomic.make [] in
    let write_first_page p = Atomic.set first_pages (p::(Atomic.get first_pages)) in
    let enc = create comments params write_first_page serialno in
    Gc.finalise finalize_encoder_private_values enc;
    set_write_cb enc write_cb;
    { encoder = Obj.magic (enc, params); callbacks = Obj.magic (Flac.Encoder.get_callbacks (fun _ -> raise Flac.Internal));
      first_pages = List.rev (Atomic.get first_pages) }
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
