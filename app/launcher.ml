
type compiler = BYTE | OPT
type target = STANDALONE | DYNAMIC

let execute cmd =
  let result = Sys.command cmd
  in if result = 0 then ()
  else failwith (Printf.sprintf "Command '%s' failed with error code %d" cmd result)

let build_dir () =
  Filename.concat (Filename.get_temp_dir_name ()) ("p5ml" ^ Filename.dir_sep)

let check_file filename =
  if Sys.file_exists filename then () else failwith "File does not exist!";
  if Sys.is_directory filename then failwith "File is a directory!" else ();
  if Filename.check_suffix filename ".ml" then () else failwith "Must specify a .ml file!";
  ()

let make_sketch_dir () =
  let dir = build_dir ()
  in execute (Printf.sprintf "rm -rf %s && mkdir -p %s && cd %s" dir dir dir)

let chop_filename filename =
  Filename.chop_suffix (Filename.basename filename) ".ml"

let compile compiler target filename =
  let dir = build_dir ()
  in let ext = match target, compiler with
      | STANDALONE, BYTE -> "byte"
      | STANDALONE, OPT -> "native"
      | DYNAMIC, BYTE -> "cma"
      | DYNAMIC, OPT -> "cmxs"
  in let base_filename = chop_filename filename
  in let command =
       Printf.sprintf ("cp %s %s && cd %s && "
                       ^^ "ocamlbuild -use-ocamlfind -package p5ml -tag thread %s.%s")
         filename dir dir base_filename ext
  in execute command; Unix.symlink (Filename.concat dir (Printf.sprintf "_build/%s.%s" base_filename ext))
    (Filename.concat dir "sketch")

let launch_sketch () =
  let dir = build_dir ()
  in ignore (Sys.command (Printf.sprintf "cd %s && .%ssketch" dir Filename.dir_sep))

(* note: Dynlink requires each module to have a different file name *)
let dynamic_sketch filename =
  let plugin = Filename.concat (build_dir ()) "sketch" in
  (* The sketch cma/cmxs is unsafe because it uses libraries that have external functions;
     dynamically loaded external functions can break type safety. We should be safe though. *)
  Dynlink.allow_unsafe_modules true;
  Runner.Dynamic.set_dynamic ();
  Dynlink.loadfile plugin

let launch compiler target filename =
  check_file filename;
  make_sketch_dir ();
  compile compiler target filename;
  match target with
  | STANDALONE -> launch_sketch ()
  | DYNAMIC -> dynamic_sketch filename

exception ParseCmdError

let parse_cmd () =
  let dynamic = ref STANDALONE
  in let file_opt : string option ref = ref None
  in let usage = "Usage: p5ml file [-d]"
  in let spec = Arg.align [
      ("-d", Arg.Unit (fun () -> dynamic := DYNAMIC), "\tDynamic");
    ]
  in Arg.parse spec (fun arg -> file_opt := Some arg) usage;
  match !file_opt with
  | Some file -> file, !dynamic
  | None -> Arg.usage spec usage; raise ParseCmdError

let () =
  try
    let compiler = if Dynlink.is_native then OPT else BYTE
    in let file, target = parse_cmd ()
    in launch compiler target file
  with
  | End_of_file | ParseCmdError -> ()
  | Failure msg -> prerr_endline ("Error: " ^ msg)
