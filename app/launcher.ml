
type compiler = BYTE | OPT

let execute cmd =
  let result = Sys.command cmd
  in if result = 0 then ()
  else failwith (Printf.sprintf "Command '%s' failed with error code %d" cmd result)

let build_dir () =
  Filename.get_temp_dir_name () ^ Filename.dir_sep ^ "p5ml/"

let check_file filename =
  if Sys.file_exists filename then () else failwith "File does not exist!";
  if Sys.is_directory filename then failwith "File is a directory!" else ();
  if Filename.check_suffix filename ".ml" then () else failwith "Must specify a .ml file!";
  ()

let make_sketch_dir () =
  let dir = build_dir ()
  in execute (Printf.sprintf "rm %s -rf && mkdir -p %s && cd %s" dir dir dir)

let compile comp filename =
  let dir = build_dir ()
  in let compiler, ext = match comp with
    | BYTE -> "ocamlopt", "cmxa"
    | OPT -> "ocamlc", "cma"
  in let base_filename = Filename.chop_suffix (Filename.basename filename) ".ml"
  in let command =
       Printf.sprintf
         ("cp %s %s && cd %s "
          ^^ "&& ocamlfind %s -package p5ml unix.%s graphics.%s p5.%s %s.ml -o sketch ")
         filename dir dir compiler ext ext ext base_filename
  in execute command

let launch_sketch () =
  let dir = build_dir ()
  in ignore (Sys.command (Printf.sprintf "cd %s && ./sketch" dir))

let launch filename =
  check_file filename;
  make_sketch_dir ();
  compile OPT filename;
  launch_sketch ()

let () =
  try
    if Array.length Sys.argv >= 2
    then launch Sys.argv.(1)
    else failwith "Must specify filename!"
  with
  | End_of_file -> ()
  | Failure msg -> prerr_endline ("Error: " ^ msg)
