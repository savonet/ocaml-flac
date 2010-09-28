
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

let fd = 
  Printf.printf "Opening input file %S\n%!" !infile;
  Unix.openfile !infile [Unix.O_RDONLY] 0o640

let od = 
  Printf.printf "Opening output file %S\n%!" !outfile;
  open_out !outfile

let read_f n =
  let s = String.create n in
  let ret = Unix.read fd s 0 n in
  s,ret

let rec decode decoder = 
  let ret = Flac.read_pcm decoder in
  output_string od ret ; flush od;
  if not (Flac.is_eos decoder) then
    decode decoder 

let () = 
  let decoder = Flac.create read_f () () in
  decode decoder ;
  Gc.full_major ()

