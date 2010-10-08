
module Decoder = 
struct

  type ogg

  let get_callbacks write : ogg Flac.Decoder.callbacks = 
      Obj.magic 
       (Flac.Decoder.get_callbacks 
         (fun _ -> raise Flac.Internal)
         write)

  external check_packet : Ogg.Stream.packet -> bool = "ocaml_flac_decoder_check_ogg"

  external create : Ogg.Stream.packet -> Ogg.Stream.t -> ogg Flac.Decoder.callbacks -> ogg Flac.Decoder.dec = "ocaml_flac_decoder_ogg_create"

end

module Encoder = 
struct

  type ogg

  type enc

  let callbacks : ogg Flac.Encoder.callbacks = 
    Obj.magic
     (Flac.Encoder.get_callbacks (fun _ -> raise Flac.Internal))

  type init_c = Ogg.Stream.packet -> unit

  external create : (string * string) array -> Flac.Encoder.params -> Ogg.Stream.t -> init_c -> enc = "ocaml_flac_encoder_ogg_create"

  let create ?(comments=[]) params os =
    if params.Flac.Encoder.channels <= 0 then
      raise Flac.Encoder.Invalid_data ;
    let comments = Array.of_list comments in
    let ret = Queue.create () in
    let init_c p = 
      Queue.push p ret
    in
    let enc = create comments params os init_c in
    let rec f acc = 
      try
        f ((Queue.pop ret)::acc)
      with
        | Queue.Empty -> 
            begin
              match List.rev acc with
                | [] -> raise Flac.Internal
                | x :: l -> x,l
            end
    in
    let p,l = f [] in
    Obj.magic (enc,params),p,l

  external finish : enc -> unit = "ocaml_flac_encoder_ogg_finish"

  let finish e = 
    let (e,p) = Obj.magic e in
    finish e

end

module Skeleton =
struct

  external fisbone : Nativeint.t -> Int64.t -> Int64.t -> string -> Ogg.Stream.packet = "ocaml_flac_skeleton_fisbone"

  let fisbone ?(start_granule=Int64.zero)
              ?(headers=["Content-type","audio/x-flac"])
              ~serialno ~samplerate () =
    let concat s (h,v) =
      Printf.sprintf "%s%s: %s\r\n" s h v
    in
    let s =
      List.fold_left concat "" headers
    in
    fisbone serialno samplerate start_granule s

end

