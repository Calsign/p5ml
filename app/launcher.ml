
type compiler = BYTE | OPT
type target = STANDALONE | DYNAMIC

let unique_num : unit -> int =
  let num = ref 0
  in fun () -> let v = !num in num := v + 1; v

exception Execution_failure of string

(** [execute ?wd cmd] executes the command [cmd] in working directory [wd], or
    [Unix.getcwd ()] by default. Raises [Failure msg] if the command returns a
    non-zero exit code. *)
let execute ?wd cmd =
  let result =
    match wd with
    | None -> Sys.command cmd
    | Some new_wd ->
      begin
        let old_wd = Unix.getcwd ()
        in Unix.chdir new_wd;
        let res = Sys.command cmd
        in Unix.chdir old_wd; res
      end
  in if result = 0 then ()
  else raise (Execution_failure
                (Printf.sprintf
                   "Command '%s' failed with error code %d" cmd result))

(** [build_dir ()] is the temporary directory in which the launcher builds
    sketches. *)
let build_dir () =
  Filename.concat (Filename.get_temp_dir_name ()) ("p5ml" ^ Filename.dir_sep)

(** [check_file file] raises [Failure msg] if file is not a valid .ml file. *)
let check_file filename =
  if Sys.file_exists filename then () else failwith "File does not exist!";
  if Sys.is_directory filename then failwith "File is a directory!" else ();
  if Filename.check_suffix filename ".ml" then ()
  else failwith "Must specify a .ml file!"; ()

(** [delete_dir dir] recursively deletes the directory [dir]. *)
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

(** [make_sketch_dir ()] deletes the old build directory and returns the path
    to a newly created build directory. *)
let make_sketch_dir () =
  let dir = build_dir () in delete_dir dir; Unix.mkdir dir 0o755

(** [chop_filename filename] is the name of [filename] without the extension,
    e.g. [chop_filename "foo/bar.ml"] is ["bar"]. *)
let chop_filename filename =
  Filename.chop_suffix (Filename.basename filename) ".ml"

(** [copy_sketch_file file dest wrap] copies [file] to [dest]. If [wrap] is
    specified, then the sketch is wrapped in boilerplate code: open P5.Gtkc,
    declare module, and include Base at the beginning, close module and run
    sketch at the end. The copy is a buffered binary copy, so it is fast. *)
let copy_sketch_file in_file out_file wrap =
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

(** [compile compiler target wrap update filename] compiles [filename] using
    the specified [compiler] (bytecode or native) and [target] (standalone or
    dynamic). See [copy_sketch_file] for an explanation of [wrap]. If [update]
    (dynamic mode only), then the old build folder is preserved and this build
    is treated as an update to a currently-running dynamic sketch. *)
let compile compiler target wrap update filename =
  let dir = build_dir ()
  in let ext = match target, compiler
       with
      | STANDALONE, BYTE -> "byte"
      | STANDALONE, OPT -> "native"
      | DYNAMIC, BYTE -> "cma"
      | DYNAMIC, OPT -> "cmxs"
  in let base_filename = chop_filename filename
  in let build_ml_file = Filename.concat dir
         (Printf.sprintf "%s.ml" base_filename)
  in let output_filename = Printf.sprintf "%s.%s" base_filename ext
  in let output_file = Filename.concat dir
         (Filename.concat "_build" output_filename)
  (* Dynlink requires each module to have a different file name,
     so we use a unique identifier every time we reload the sketch *)
  in let num = unique_num ()
  in let get_symlink_file n =
       let filename = Printf.sprintf "%s%u.%s" base_filename n ext
       in Filename.concat dir filename
  in let symlink_file = get_symlink_file num
  (* this command gets run in the build directory [dir] *)
  in let command = Printf.sprintf
         "ocamlbuild -use-ocamlfind -package p5ml -tag thread %s"
         output_filename
  in if update then
    begin
      (* delete the old symlink *)
      let old = get_symlink_file (num - 1)
      in if Sys.file_exists old
      then Sys.remove old else ()
    end else ();
  (* move the sketch into the build directory *)
  copy_sketch_file filename build_ml_file wrap;
  (* compile the sketch with ocamlbuild *)
  execute ~wd:dir command;
  (* make a symlink with a unique name *)
  Unix.symlink output_file symlink_file;
  (* return the file *)
  symlink_file

(** [launch_sketch file] launches the standalone sketch executable [file]. *)
let launch_sketch filename = ignore (Sys.command filename)

(** [dynamic_sketch plugin handler] launches the dynamic sketch library [file]
    using the dynamic handler [handler]. *)
let dynamic_sketch plugin handler =
  (* The sketch cma/cmxs is unsafe because it uses libraries that have
     external functions; dynamically loaded external functions can break type
     safety. We should be safe though. *)
  Dynlink.allow_unsafe_modules true;
  Runner.Dynamic.set_dynamic_handler handler;
  Dynlink.loadfile plugin

(** [launch compiler target wrap inotify update filename] compiles and launches
    the sketch [filename]. Raises [Failure msg] upon failure. *)
let rec launch compiler target wrap inotify update filename =
  check_file filename;
  if update then () else make_sketch_dir ();
  try begin
    let compiled_file = compile compiler target wrap update filename
    in match target with
    | STANDALONE -> launch_sketch compiled_file
    | DYNAMIC ->
      begin
        Runner.Dynamic.set_change_notifier inotify;
        dynamic_sketch compiled_file
          (fun () -> launch compiler target wrap inotify true filename)
      end
  end with
  | Execution_failure _
    -> prerr_endline "Cannot launch sketch, compilation failed!"

(** [realpath filename] is the canonical version of [filename]. This is useful
    because it resolves [.], [..], and symbolic links. *)
external realpath : string -> string = "caml_realpath"

(** [build_inotify file] is a function that returns [true] when [file] has been
    modified since the last invocation and [false] otherwise. *)
let build_inotify file : (unit -> bool) =
  let base_name = Filename.basename file
  (* we need to convert to canonical path *)
  in let real_file = realpath (Filename.concat (Sys.getcwd ()) file)
  in let dirname = Filename.dirname real_file
  in let open Lwt
  in Lwt_main.run
    begin
      Lwt_inotify.create () >>= fun inotify ->
      (* Detect modifications to all files in the parent directory of the file.
         There is no way to monitor just one file. *)
      Lwt_inotify.add_watch inotify dirname [Inotify.S_Modify] >>= fun watch ->
      Lwt.return
        begin
          (* [try_read] will return immediately.  *)
          fun () -> match Lwt_inotify.try_read inotify |> Lwt.state with
            (* inotify gives us the file base name, e.g. "sketch.ml" *)
            | Lwt.Return (Some (_, _, _, Some fname)) -> fname = base_name
            | _ -> false
        end
    end

exception Parse_cmd_error

(** [parse_cmd] is [(file, target, wrap)], the result of processing the
    command-line arguments supplied to the launcher. [file] is the sketch
    file to compile, [target] is standalone or dynamic, and [wrap] is whether
    or not to wrap the sketch code (see [copy_sketch_file]). *)
let parse_cmd () =
  let target = ref STANDALONE
  in let file_opt : string option ref = ref None
  in let wrap = ref false
  in let usage = "Usage: p5ml file [-d] [-w]"
  in let spec = Arg.align [
      ("-d", Arg.Unit (fun () -> target := DYNAMIC), "\tDynamic mode");
      ("-w", Arg.Unit (fun () -> wrap := true),
       "\tPerform preprocessor wrapping");
    ]
  in Arg.parse spec (fun arg -> file_opt := Some arg) usage;
  match !file_opt with
  | Some file -> file, !target, !wrap
  | None -> Arg.usage spec usage; raise Parse_cmd_error

let () =
  try
    (* detect bytecode or native *)
    let compiler = if Dynlink.is_native then OPT else BYTE
    (* parse command-line input *)
    in let file, target, wrap = parse_cmd ()
    (* detect changes to the source file *)
    in let inotify = build_inotify file
    (* build and launch the sketch *)
    in launch compiler target wrap inotify false file
  with
  | End_of_file | Parse_cmd_error -> ()
  | Failure msg when msg = "realpath" -> prerr_endline "Error: Invalid file"
  | Failure msg -> prerr_endline ("Error: " ^ msg)
