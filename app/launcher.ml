
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
  in if result <> 0 then
    raise (Execution_failure
             (Printf.sprintf
                "Command '%s' failed with error code %d" cmd result))

(** [build_dir ()] is the temporary directory in which the launcher builds
    sketches. *)
let build_dir () =
  Filename.concat (Filename.get_temp_dir_name ()) ("p5ml" ^ Filename.dir_sep)

(** [check_file file] raises [Failure msg] if file is not a valid .ml file. *)
let check_file filename =
  if not (Sys.file_exists filename) then
    failwith "File does not exist!";
  if Sys.is_directory filename then
    failwith "File is a directory!";
  if not (Filename.check_suffix filename ".ml") then
    failwith "Must specify a .ml file!"

(** [delete_dir dir] recursively deletes the directory [dir]. *)
let rec delete_dir dir =
  if Sys.file_exists dir then
    match (Unix.stat dir).st_kind with
    | S_DIR ->
      begin
        Array.iter
          (fun fname -> delete_dir (Filename.concat dir fname))
          (Sys.readdir dir);
        Unix.rmdir dir
      end
    | _ -> Sys.remove dir

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
    if wrap then output_string writer wrap_begin;
    handle_read ();
    if wrap then output_string writer wrap_end;
    close_in reader;
    close_out writer
  end

(** [compile compiler target wrap update filenames] compiles [filenames] using
    the specified [compiler] (bytecode or native) and [target] (standalone or
    dynamic). See [copy_sketch_file] for an explanation of [wrap]. If [update]
    (dynamic mode only), then the old build folder is preserved and this build
    is treated as an update to a currently-running dynamic sketch. *)
let compile compiler target wrap update filenames =
  if List.length filenames < 1 then failwith "Must specify file";

  (* As of OCaml 4.08.0, Dynlink fails when attempting to load a module with
     the same name as one that has already been loaded, which provides more
     soundness. Unfortunately, loading multiple modules with the same name is
     the name of the game for dynamic mode. We can get around this by changing
     the sketch filename to something different every time, which works great
     for the main file. The problem is that secondary dependency files cannot
     have their names changed in this manner because doing so would cause any
     code referencing them to break, because the compiled module name is
     determined from the file name. Therefore we are not able to support
     multiple files in dynamic mode for now. *)
  if target = DYNAMIC && List.length filenames > 1 then
    failwith "Dynamic mode only supports one file";

  let dir = build_dir ()
  in let ext = match target, compiler with
      | STANDALONE, BYTE -> "byte"
      | STANDALONE, OPT -> "native"
      | DYNAMIC, BYTE -> "cma"
      | DYNAMIC, OPT -> "cmxs"

  (* Dynlink requires each module to have a different file name,
     so we use a unique identifier every time we reload the sketch *)
  in let num = unique_num ()
  in let main_filename_base = chop_filename (List.hd filenames)
  in let main_output_filename =
       Printf.sprintf "%s%u.%s" main_filename_base num ext
  in let main_output_file =
       Filename.concat dir (Filename.concat "_build" main_output_filename)

  (* copy a file from sketch folder to build directory, changing name as
     necessary for Dynlink *)
  in let prepare_file main file =
       let chopped = chop_filename file
       in let file_name =
            (* we only rename the main file *)
            if main then Printf.sprintf "%s%u.ml" chopped num
            else Printf.sprintf "%s.ml" chopped
       in let dest = Filename.concat dir file_name
       (* we only wrap the main file *)
       in copy_sketch_file file dest (wrap && main)

  (* build command, runs in build directory *)
  in let command = Printf.sprintf
         "ocamlbuild -use-ocamlfind -package p5ml -tag thread %s"
         main_output_filename

  (* copy sketch files to build directory *)
  in prepare_file true (List.hd filenames);
  List.iter (prepare_file false) (List.tl filenames);
  (* compile the sketch with ocamlbuild *)
  execute ~wd:dir command;
  (* return the file *)
  main_output_file

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

(** [launch compiler target wrap inotify update filenames] compiles and launches
    the sketch specified by [filenames], where [List.hd filenames] is the main
    sketch file. Raises [Failure msg] upon failure. *)
let rec launch compiler target wrap inotify update filenames =
  match filenames with | [] -> failwith "No files specified" | _ -> ();
  List.iter check_file filenames;
  if update then () else make_sketch_dir ();
  try begin
    let compiled_file = compile compiler target wrap update filenames
    in match target with
    | STANDALONE -> launch_sketch compiled_file
    | DYNAMIC ->
      begin
        Runner.Dynamic.set_change_notifier inotify;
        dynamic_sketch compiled_file
          (fun () -> launch compiler target wrap inotify true filenames)
      end
  end with
  | Execution_failure _
    -> prerr_endline "Cannot launch sketch, compilation failed!"

(** [build_inotify files] is a function that returns [true] when any file in
    [files] has been modified since the last invocation and [false]
    otherwise. *)
let build_inotify files : (unit -> bool) =
  (* We used to use inotify, but that's not available outside Linux.
     We can just use Digests. *)
  let digests_of_files files =
    List.map Digest.file files
  in let compare_digests digests_a digests_b =
       List.map2 (fun a b -> Digest.compare a b <> 0) digests_a digests_b
       |> List.fold_left (||) false
  in let digests = ref (digests_of_files files)
  in fun () ->
    begin
      let digests' = digests_of_files files
      in let result = compare_digests !digests digests'
      in digests := digests'; result
    end

exception Parse_cmd_error

(** [parse_cmd] is [(files, target, wrap)], the result of processing the
    command-line arguments supplied to the launcher. [files] is the list of
    sketch files to compile, [target] is standalone or dynamic, and [wrap] is
    whether or not to wrap the sketch code (see [copy_sketch_file]). *)
let parse_cmd () =
  let target = ref STANDALONE
  in let files : string list ref = ref []
  in let wrap = ref false
  in let usage = "Usage: p5ml [-d] [-w] file ..."
  in let spec = Arg.align [
      ("-d", Arg.Unit (fun () -> target := DYNAMIC), "\tDynamic mode");
      ("-w", Arg.Unit (fun () -> wrap := true),
       "\tPerform preprocessor wrapping");
    ]
  in Arg.parse spec (fun arg -> files := arg :: !files) usage;
  match List.length !files with
  | 0 -> Arg.usage spec usage; raise Parse_cmd_error
  | _ -> List.rev !files, !target, !wrap

let () =
  try
    (* detect bytecode or native *)
    let compiler = if Dynlink.is_native then OPT else BYTE
    (* parse command-line input *)
    in let files, target, wrap = parse_cmd ()
    (* detect changes to the source file *)
    in let inotify = build_inotify files
    (* build and launch the sketch *)
    in launch compiler target wrap inotify false files
  with
  | End_of_file | Parse_cmd_error -> ()
  | Failure msg -> prerr_endline ("Error: " ^ msg)
