
type compiler = BYTE | OPT
type target = STANDALONE | DYNAMIC

let unique_num : unit -> int =
  let num = ref 0
  in fun () -> let v = !num in num := v + 1; v

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

let rec delete_dir dir =
  match Sys.file_exists dir with
  | true ->
    begin
      match (Unix.stat dir).st_kind with
      | S_DIR ->
        begin
          Array.iter
            (fun fname -> delete_dir (Filename.concat dir fname))
            (Sys.readdir dir);
          Unix.rmdir dir
        end
      | _ -> Sys.remove dir
    end
  | false -> ()

let make_sketch_dir () =
  let dir = build_dir () in delete_dir dir; Unix.mkdir dir 0o755

let chop_filename filename =
  Filename.chop_suffix (Filename.basename filename) ".ml"
let compile compiler target filename =
  let dir = build_dir ()
  in let ext = match target, compiler
       with
      | STANDALONE, BYTE -> "byte"
      | STANDALONE, OPT -> "native"
      | DYNAMIC, BYTE -> "cma"
      | DYNAMIC, OPT -> "cmxs"
  in let base_filename = chop_filename filename
  (* Dynlink requires each module to have a different file name,
     so we use a unique identifier every time we reload the sketch *)
  in let num = unique_num ()
  in let ml_filename = base_filename ^ string_of_int num ^ ".ml"
  in let output_filename = Printf.sprintf "%s%u.%s" base_filename num ext
  in let command =
       Printf.sprintf ("cp %s %s && cd %s && "
                       ^^ "ocamlbuild -use-ocamlfind -package p5ml -tag thread %s")
         filename (Filename.concat dir ml_filename) dir output_filename
  in execute command; Filename.concat dir (Filename.concat "_build" output_filename)

let launch_sketch filename = ignore (Sys.command filename)

let dynamic_sketch plugin handler =
  (* The sketch cma/cmxs is unsafe because it uses libraries that have external functions;
     dynamically loaded external functions can break type safety. We should be safe though. *)
  Dynlink.allow_unsafe_modules true;
  Runner.Dynamic.set_dynamic_handler handler;
  Dynlink.loadfile plugin

let rec launch compiler target filename =
  check_file filename;
  make_sketch_dir ();
  let compiled_file = compile compiler target filename
  in match target with
  | STANDALONE -> launch_sketch compiled_file
  | DYNAMIC -> dynamic_sketch compiled_file (fun () -> launch compiler target filename)

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
