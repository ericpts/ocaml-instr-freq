open Core
open Ocamlcfg
open Instr_freq

let start = Time.now ()

(* Credits to
   https://github.com/janestreet/jenga/blob/c10d05423320e0a8c113b9a09674d8bc69bfbc2c/bench/run.ml#L5*)
let eprintf_progress fmt =
  ksprintf
    (fun str ->
      let from_start = Time.diff (Time.now ()) start in
      let decimals =
        match Time.Span.to_unit_of_time from_start with
        | Nanosecond | Microsecond | Millisecond -> 0
        | Second -> 1
        | Minute | Hour | Day -> 2
      in
      Out_channel.printf "[%5s] %s%!"
        (Time.Span.to_string_hum from_start ~decimals)
        str)
    fmt
;;

let read_file (file : Filename.t) =
  let open Linear_format in
  let ui, _ = restore file in
  List.filter_map ui.items ~f:(fun item ->
      match item with
      | Func f ->
          let cfg_builder =
            Cfg_builder.from_linear f ~preserve_orig_labels:false
          in
          let layout = Cfg_builder.get_layout cfg_builder in
          let blocks =
            List.map layout ~f:(fun label ->
                Cfg_builder.get_block cfg_builder label |> Option.value_exn)
          in
          Some (f, blocks)
      | Data _ -> None)
;;

let build_index files ~(index_file : Filename.t) =
  let total_number_of_files = List.length files in
  printf "Building the index...\n%!";
  let index = Index.empty () in
  let functions_per_file = ref [] in
  let blocks_per_function = ref [] in
  let instructions_per_block = ref [] in
  List.iteri files ~f:(fun ifile file ->
      eprintf_progress "Processing file %d/%d; Index load: %s \r" ifile
        total_number_of_files
        (Index.print_hashtbl_load_statistics index);
      let functions = read_file file in
      functions_per_file := List.length functions :: !functions_per_file;
      List.iter functions ~f:(fun (_fun_decl, blocks) ->
          blocks_per_function := List.length blocks :: !blocks_per_function;
          List.iter blocks ~f:(fun block ->
              Index.update index block;
              instructions_per_block :=
                (List.length block.body + 1) :: !instructions_per_block)));
  let () = Index.to_file index ~filename:index_file in
  let sum nums = List.sum (module Int) nums ~f:Fn.id in
  let mean nums =
    Float.of_int (sum nums) /. Float.of_int (List.length nums)
  in
  let max nums =
    List.max_elt nums ~compare:Int.compare |> Option.value_exn
  in
  let blocks = !blocks_per_function in
  let functions = !functions_per_file in
  let instructions = !instructions_per_block in
  printf "\nSaved index to %s\n%!" index_file;

  Out_channel.with_file (index_file ^ ".log") ~f:(fun f ->
      fprintf f "Processed:\n";
      fprintf f "\tfiles: %d\n" (List.length files);
      fprintf f "\tfunctions: total %d; mean/file %f; max/file %d \n"
        (sum functions) (mean functions) (max functions);
      fprintf f "\tblocks: total %d; mean/function %f; max/function %d \n"
        (sum blocks) (mean blocks) (max blocks);
      fprintf f "\tinstructions: total %d; mean/block %f; max/block %d\n"
        (sum instructions) (mean instructions) (max instructions);
      fprintf f "Index statistics: %s\n"
        (Index.print_hashtbl_load_statistics index));
  index
;;

exception Stop_iteration

let main
    files
    ~index_file
    ~n_real_blocks_to_print
    ~n_most_frequent_equivalences
    ~block_print_mode
    ~min_block_size
    ~min_equivalence_class_size
    ~count_equivalence_classes_of_each_size
    ~matcher_of_index =
  let index =
    if Sys.file_exists_exn index_file then (
      printf "Loading cached index from %s... %!" index_file;
      let index = Index.of_file ~filename:index_file in
      printf "loaded!\n%!";
      index )
    else build_index files ~index_file
  in
  let matcher = Option.map matcher_of_index ~f:(fun f -> f index) in
  (* XCR gyorsh for : this is a nice way to add different patterns; consider
     separating it out a bit more and explosing a type for functions that
     return a pair of on_block and on_finish_iterations. What are the
     restrictions? e.g., does order they are listed in matter / do they have
     to preserve index? *)
  let { Stats.on_block; on_finish_iteration } =
    let statistics = ref [] in
    let add_stat stat = statistics := stat :: !statistics in
    add_stat (Stats.count_blocks_matching index ~min_block_size ~matcher);

    if n_real_blocks_to_print > 0 then
      add_stat
        (Stats.print_most_popular_classes index ~n_real_blocks_to_print
           ~n_most_frequent_equivalences ~block_print_mode ~min_block_size
           ~matcher);

    if count_equivalence_classes_of_each_size then
      add_stat (Stats.count_equivalence_classes_of_each_size ());

    Stats.combine !statistics
  in
  ( try
      List.iter files ~f:(fun file ->
          List.iter (read_file file) ~f:(fun (fun_decl, blocks) ->
              List.iter blocks ~f:(fun block ->
                  if List.length block.body + 1 >= min_block_size then
                    let equivalence = Index.equivalence_exn index block in
                    let frequency = Index.frequency_exn index equivalence in
                    if frequency >= min_equivalence_class_size then
                      match
                        on_block ~file ~equivalence ~frequency block
                          fun_decl
                      with
                      | `Continue -> ()
                      | `Stop -> raise Stop_iteration)))
    with Stop_iteration -> () );
  on_finish_iteration ()
;;

let block_print_mode_arg =
  Command.Arg_type.create (fun mode ->
      match String.lowercase mode with
      | "asm" -> Types.As_assembly
      | "cfg" -> Types.As_cfg
      | "both" -> Types.Both
      | _ -> failwithf "Invalid block_print_mode argument: %s" mode ())
;;

let main_command =
  Command.basic ~summary:"Count frequency of basic blocks."
    ~readme:(fun () ->
      "Group contents of basic blocks based on equivalence classes, and \
       print the most common classes.")
    [%map_open.Command.Let_syntax
      let anon_files = anon (sequence ("input" %: Filename.arg_type))
      and n_real_blocks_to_print =
        flag "-print-n-real-blocks"
          (optional_with_default 5 int)
          ~doc:
            "n Print [n] actual blocks from the codebase for each \
             equivalence class."
      and n_most_frequent_equivalences =
        flag "-n-most-frequent-equivalences"
          (optional_with_default 10 int)
          ~doc:"n Print most frequent equivalence classes"
      and block_print_mode =
        flag "-block-print-mode"
          (required block_print_mode_arg)
          ~doc:
            "repr Which representatin to print blocks in. Must be one of \
             [asm | cfg | both]"
      and min_block_size =
        flag "-min-block-size"
          (optional_with_default 5 int)
          ~doc:
            "n Only report equivalence classes, for which the \
             representative block has at least [n] instructions (including \
             the terminator)."
      and count_equivalence_classes_of_each_size =
        flag "-count-equivalence-classes-of-each-size" no_arg
          ~doc:
            "Report statistics of the form \
             (number_of_members_in_equivalence_class, equivalence classes \
             with this many members). Might take a long time."
      and min_equivalence_class_size =
        flag "-min-equivalence-class-size"
          (optional_with_default 5 int)
          ~doc:
            "n Only report equivalence classes which have at least [n] \
             members"
      and index_file =
        flag "-index-file"
          (required Filename.arg_type)
          ~doc:
            "filepath Location of index file. Will be built if it does not \
             exist."
      and from_file =
        flag "-from-file"
          (optional Filename.arg_type)
          ~doc:
            "list-file Treat each line of [list-file] as a file to process."
      and matcher =
        flag "-use-matcher"
          (optional Filename.arg_type)
          ~doc:
            "sexp_matcher_file Print only symbolic blocks which match the \
             given matcher.\n\
            \          Example of contents:\n\
            \   ((with_these_basics ((((desc (Op Move)) (arg (0)) (res \
             (1))) ((desc (Op Spill)) (arg (1)) (res (2)))))) \
             (with_this_terminator ()))\n\n\n"
      in
      let matcher_of_index =
        Option.map matcher ~f:(fun matcher ->
            printf "Loading matcher from create_args(%s)\n%!" matcher;
            let create_args =
              Sexp.load_sexp_conv_exn matcher
                Index.Matcher.create_args_of_sexp
            in
            Index.Matcher.create create_args)
      in
      let files =
        anon_files
        @
        match from_file with
        | None -> []
        | Some file -> In_channel.read_lines file
      in
      fun () ->
        main files ~index_file ~n_real_blocks_to_print
          ~n_most_frequent_equivalences ~block_print_mode ~min_block_size
          ~min_equivalence_class_size
          ~count_equivalence_classes_of_each_size ~matcher_of_index]
;;

let () = Command.run main_command
