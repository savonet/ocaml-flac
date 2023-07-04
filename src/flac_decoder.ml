(*
 * Copyright 2003-2011 Savonet team
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

let check = Flac_ogg.Decoder.check_packet

let decoder os =
  let ogg_dec = ref None in
  let decoder = ref None in
  let write_ref = ref (fun _ -> ()) in
  let write ret =
    let fn = !write_ref in
    fn ret
  in
  let callbacks_ref = ref (Flac_ogg.Decoder.get_callbacks os write) in
  let get_decoder () =
    match !decoder with
      | None ->
          let ogg_dec =
            match !ogg_dec with
              | None ->
                  let dec = Flac.Decoder.create !callbacks_ref in
                  ogg_dec := Some dec;
                  dec
              | Some dec -> dec
          in
          let dec, info, m = Flac.Decoder.init ogg_dec !callbacks_ref in
          let meta =
            match m with None -> ("Unknown vendor", []) | Some x -> x
          in
          decoder := Some (dec, info, meta);
          (dec, info, meta)
      | Some d -> d
  in
  let info () =
    let _, info, m = get_decoder () in
    ( {
        Ogg_decoder.channels = info.Flac.Decoder.channels;
        sample_rate = info.Flac.Decoder.sample_rate;
      },
      m )
  in
  let decode feed =
    write_ref := feed;
    let decoder, _, _ = get_decoder () in
    match Flac.Decoder.state decoder !callbacks_ref with
      | `Search_for_metadata | `Read_metadata | `Search_for_frame_sync
      | `Read_frame ->
          Flac.Decoder.process decoder !callbacks_ref
      (* Ogg decoder is responsible for detecting end of stream vs. end of track. *)
      | _ -> raise Ogg.Not_enough_data
  in
  let restart new_os =
    (write_ref := fun _ -> ());
    let d, _, _ = get_decoder () in
    (* Flush error are very unlikely. *)
    assert (Flac.Decoder.flush d !callbacks_ref);
    callbacks_ref := Flac_ogg.Decoder.get_callbacks new_os write
  in
  Ogg_decoder.Audio
    {
      Ogg_decoder.name = "flac";
      info;
      decode;
      restart;
      samples_of_granulepos = (fun x -> x);
    }

let register () = Hashtbl.add Ogg_decoder.ogg_decoders "flac" (check, decoder)
