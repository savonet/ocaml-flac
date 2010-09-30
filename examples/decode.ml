
let output_int chan n =
  output_char chan (char_of_int ((n lsr 0) land 0xff));
  output_char chan (char_of_int ((n lsr 8) land 0xff));
  output_char chan (char_of_int ((n lsr 16) land 0xff));
  output_char chan (char_of_int ((n lsr 24) land 0xff))


let output_short chan n =
  output_char chan (char_of_int ((n lsr 0) land 0xff));
  output_char chan (char_of_int ((n lsr 8) land 0xff))

let progress_bar =
  let spin = ref 0 in
    (
      fun title pos tot ->
        let nbeq = 40 in
        let n = min (100. *. (float_of_int pos) /. (float_of_int tot)) 100. in
        let e = int_of_float (n /. 100. *. (float_of_int nbeq)) in
          Printf.printf "\r%s %6.2f%% [" title n;
          for i = 1 to e do Printf.printf "=" done;
          if e != nbeq then Printf.printf ">";
          for i = e + 2 to nbeq do Printf.printf " " done;
          Printf.printf "] ";
          incr spin;
          if !spin > 4 then spin := 1;
          Printf.printf "%c%!"
            (
              if n = 100. then ' '
              else
                match !spin with
                  | 1 -> '|'
                  | 2 -> '/'
                  | 3 -> '-'
                  | 4 -> '\\'
                  | _ -> failwith "this did not happen"
            )
    )

let infile = ref "input.flac"
let outfile = ref "output.raw"

let () =
  Arg.parse
    [
      "-o", Arg.Set_string outfile, "Output file";
      "-i", Arg.Set_string infile, "Input file";
    ]
    ignore
    "decode [options]"

let () =
  let fd =
    Printf.printf "Opening input file %S\n%!" !infile;
    Unix.openfile !infile [Unix.O_RDONLY] 0o640
  in
  let oc =
    Printf.printf "Opening output file %S\n%!" !outfile;
    open_out !outfile
  in
  let read_f n =
    let s = String.create n in
    let ret = Unix.read fd s 0 n in
    s,ret
  in 
  let decoder = Flac.create read_f () () in
  let info = 
    match Flac.info decoder with
      | Some info ->
         Printf.printf "Stream info:\n";
         Printf.printf "sample rate: %i\n" info.Flac.sample_rate ;
         Printf.printf "bits per sample: %i\n" info.Flac.bits_per_sample ;
         Printf.printf "channels: %i\n" info.Flac.channels ;
         Printf.printf "total samples: %s\n" (Int64.to_string info.Flac.total_samples) ;
         Printf.printf "md5sum: " ;
         String.iter (fun c -> Printf.printf "%x" (int_of_char c)) info.Flac.md5sum ;
         Printf.printf "\n";
         info
      | None -> failwith "No info for stream: is it a valid FLAC file?"
  in
  if info.Flac.bits_per_sample <> 16 then
    failwith "Unsupported bits per sample." ;
  let srate = 
    info.Flac.sample_rate
  in
  let chans = 
    info.Flac.channels
  in
  let datalen = 
    (Int64.to_int info.Flac.total_samples) * chans * 2
  in
  output_string oc "RIFF";
  output_int oc (4 + 24 + 8 + datalen);
  output_string oc "WAVE";
  output_string oc "fmt ";
  output_int oc 16;
  output_short oc 1; (* WAVE_FORMAT_PCM *)
  output_short oc chans; (* channels *)
  output_int oc srate; (* freq *)
  output_int oc (srate * chans * 2); (* bytes / s *)
  output_short oc (chans * 2); (* block alignment *)
  output_short oc 16; (* bits per sample *)
  output_string oc "data";
  output_int oc datalen;
  let pos = ref 0 in
  let rec decode () =
    let ret = Flac.read_pcm decoder in
    pos := !pos + (String.length ret) ;
    progress_bar "Decoding FLAC file:" !pos datalen ;
    output_string oc ret ;
    match Flac.state decoder with
      | `End_of_stream -> Printf.printf "\n"
      | _ -> decode ()
  in 
  decode () ;
  close_out oc ;
  Unix.close fd ;
  Gc.full_major ()

