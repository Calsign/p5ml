
type compiler = BYTE | OPT
type target = STANDALONE | DYNAMIC

let unique_num : unit -> int =
  let num = ref 0
  in fun () -> let v = !num in num := v + 1; v

exception Execution_failure of string

let execute cmd =
  let result = Sys.command cmd
  in if result = 0 then ()
  else raise (Execution_failure
                (Printf.sprintf "Command '%s' failed with error code %d" cmd result))

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

let copy_file in_file out_file wrap =
  let wrap_begin = "open P5.Gtkc\nmodule S = struct\ninclude Base\n"
  in let wrap_end = "end\nlet () = run_sketch (module S)"
  in let reader = open_in in_file
  in let writer = open_out out_file
  in let len = 4096
  in let buff = Bytes.create len
  in let rec handle_read () =
       match input reader buff 0 len with
       | 0 -> ()
       | read -> output writer buff 0 read; handle_read ()
  in begin
    if wrap then output_string writer wrap_begin else ();
    handle_read ();
    if wrap then output_string writer wrap_end else ();
    close_in reader;
    close_out writer
  end

let compile compiler target wrap filename =
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
  in let command = Printf.sprintf
         "cd %s && ocamlbuild -use-ocamlfind -package p5ml -tag thread %s"
         dir output_filename
  in copy_file filename (Filename.concat dir ml_filename) wrap;
  execute command;
  Filename.concat dir (Filename.concat "_build" output_filename)

let launch_sketch filename = ignore (Sys.command filename)

let dynamic_sketch plugin handler =
  (* The sketch cma/cmxs is unsafe because it uses libraries that have external functions;
     dynamically loaded external functions can break type safety. We should be safe though. *)
  Dynlink.allow_unsafe_modules true;
  Runner.Dynamic.set_dynamic_handler handler;
  Dynlink.loadfile plugin

let rec launch compiler target wrap inotify filename =
  check_file filename;
  make_sketch_dir ();
  try begin
    let compiled_file = compile compiler target wrap filename
    in match target with
    | STANDALONE -> launch_sketch compiled_file
    | DYNAMIC ->
      begin
        Runner.Dynamic.set_change_notifier inotify;
        dynamic_sketch compiled_file (fun () -> launch compiler target wrap inotify filename)
      end
  end with
  | Execution_failure _ -> prerr_endline "Cannot launch sketch, compilation failed!"

external realpath : string -> string = "caml_realpath"

let build_inotify file : (unit -> bool) =
  let base_name = Filename.basename file
  (* we need to convert to canonical path *)
  in let real_file = realpath (Filename.concat (Sys.getcwd ()) file)
  in let dirname = Filename.dirname real_file
  in let open Lwt
  in Lwt_main.run
    begin
      Lwt_inotify.create () >>= fun inotify ->
      Lwt_inotify.add_watch inotify dirname [Inotify.S_Modify] >>= fun watch ->
      Lwt.return
        begin
          fun () -> match Lwt_inotify.try_read inotify |> Lwt.state with
            (* inotify gives us the file base name, e.g. "sketch.ml" *)
            | Lwt.Return (Some (_, _, _, Some fname)) -> fname = base_name
            | _ -> false
        end
    end

exception Parse_cmd_error

let parse_cmd () =
  let dynamic = ref STANDALONE
  in let file_opt : string option ref = ref None
  in let wrap = ref false
  in let usage = "Usage: p5ml file [-d] [-w]"
  in let spec = Arg.align [
      ("-d", Arg.Unit (fun () -> dynamic := DYNAMIC), "\tDynamic mode");
      ("-w", Arg.Unit (fun () -> wrap := true), "\tPerform preprocessor wrapping");
    ]
  in Arg.parse spec (fun arg -> file_opt := Some arg) usage;
  match !file_opt with
  | Some file -> file, !dynamic, !wrap
  | None -> Arg.usage spec usage; raise Parse_cmd_error

let () =
  try
    let compiler = if Dynlink.is_native then OPT else BYTE
    in let file, target, wrap = parse_cmd ()
    in let inotify = build_inotify file
    in launch compiler target wrap inotify file
  with
  | End_of_file | Parse_cmd_error -> ()
  | Failure msg -> prerr_endline ("Error: " ^ msg)
