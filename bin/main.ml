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

exception Stop_iteration

let with_blocks_of_file (file : Filename.t) ~(f : Cfg.block -> unit) =
  let open Linear_format in
  let ui, _ = restore file in
  let functions =
    List.filter_map ui.items ~f:(fun item ->
        match item with
        | Func f ->
            Some (Cfg_builder.from_linear f ~preserve_orig_labels:false)
        | Data _ -> None)
  in
  List.iter functions ~f:(fun cfg_builder ->
      let layout = Cfg_builder.get_layout cfg_builder in
      List.iter layout ~f:(fun label ->
          let block =
            Cfg_builder.get_block cfg_builder label |> Option.value_exn
          in
          f block))
;;

let build_index files ~(index_file : Filename.t) =
  let total_number_of_files = List.length files in
  printf "Building the index...\n%!";
  let index = Index.empty () in
  List.iteri files ~f:(fun ifile file ->
      eprintf_progress "Processing file %d/%d; Index load: %s \r" ifile
        total_number_of_files (Index.print_load index);
      with_blocks_of_file file ~f:(fun block -> Index.update index block));
  let () = Index.to_file index ~filename:index_file in
  printf "\nSaved index to %s\n%!" index_file;
  index
;;

let main files ~index_file ~max_representatives_per_equivalence
    ~n_most_frequent_equivalences ~block_print_mode ~min_block_size
    ~min_equivalence_class_size =
  let index =
    if Sys.file_exists_exn index_file then (
      printf "Loading cached index from %s...%!" index_file;
      let index = Index.of_file ~filename:index_file in
      printf " loaded!\n%!";
      index )
    else build_index files ~index_file
  in
  Index.print_most_frequent index ~min_block_size
    ~n_most_frequent_equivalences;

  (* XCR gyorsh for : this is a nice way to add different patterns; consider
     separating it out a bit more and explosing a type for functions that
     return a pair of on_block and on_finish_iterations. What are the
     restrictions? e.g., does order they are listed in matter / do they have
     to preserve index? *)
  let { Stats.on_block; on_finish_iteration } =
    Stats.combine
      [ Stats.print_most_popular_classes index
          ~max_representatives_per_equivalence ~n_most_frequent_equivalences
          ~block_print_mode ~min_block_size;
        Stats.count_equivalence_classes_of_each_size ()
      ]
  in
  ( try
      List.iter files ~f:(fun file ->
          with_blocks_of_file file ~f:(fun block ->
              if List.length block.body + 1 >= min_block_size then
                let equivalence = Index.equivalence_exn index block in
                let frequency = Index.frequency_exn index equivalence in
                if frequency >= min_equivalence_class_size then
                  match on_block block ~equivalence ~frequency with
                  | `Continue -> ()
                  | `Stop -> raise Stop_iteration))
    with Stop_iteration -> () );

  on_finish_iteration ()
;;

let main_command =
  Command.basic ~summary:"Count frequency of basic blocks."
    ~readme:(fun () ->
      "Group contents of basic blocks based on equivalence classes, and \
       print the most common classes.")
    [%map_open.Command.Let_syntax
      let files = anon (sequence ("input" %: Filename.arg_type))
      and max_representatives_per_equivalence =
        flag "-max-representatives-per-equivalence"
          (optional_with_default 5 int)
          ~doc:"n Print [n] representatives for each equivalence class"
      and n_most_frequent_equivalences =
        flag "-n-most-frequent-equivalences"
          (optional_with_default 10 int)
          ~doc:"n Print most frequent equivalence classes"
      and print_blocks_as_assembly =
        flag "-print-block-as-assembly" no_arg
          ~doc:"Print blocks as assembly instead of cfg format"
      and min_block_size =
        flag "-min-block-size"
          (optional_with_default 5 int)
          ~doc:
            "n Only report equivalence classes, for which the \
             representative block has at least [n] instructions."
      and min_equivalence_class_size =
        flag "-min-equivalence-class-size"
          (optional_with_default 5 int)
          ~doc:
            "n Only report equivalence classes which have at least [n] \
             members"
      and index_file =
        flag "-index-file"
          (required Filename.arg_type)
          ~doc:"Location of index file. Will be built if it does not exist."
      and from_file =
        flag "-from-file" no_arg
          ~doc:
            "Treat the file argument as containing the list of files to be \
             processed."
      in
      let block_print_mode =
        match print_blocks_as_assembly with
        | true -> `As_assembly
        | false -> `As_cfg
      in
      let files =
        if from_file then
          if List.length files <> 1 then
            failwith
              "Argument -from-file requires that the program be passed a \
               single file!"
          else In_channel.read_lines (List.hd_exn files)
        else files
      in
      fun () ->
        main files ~index_file ~max_representatives_per_equivalence
          ~n_most_frequent_equivalences ~block_print_mode ~min_block_size
          ~min_equivalence_class_size]
;;

let () = Command.run main_command
