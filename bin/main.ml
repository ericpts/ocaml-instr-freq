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

let blocks_of_file (file : Filename.t) =
  let open Linear_format in
  let items = restore file in
  let functions =
    List.filter_map items ~f:(fun item ->
        restore_item item;
        match item with
        | Func d ->
            d.decl
            |> Cfg_builder.from_linear ~preserve_orig_labels:false
            |> Some
        | Data _ -> None)
  in
  List.concat_map functions ~f:(fun cfg_builder ->
      let layout = Cfg_builder.get_layout cfg_builder in
      List.map layout ~f:(fun label ->
          Cfg_builder.get_block cfg_builder label |> Option.value_exn))
;;

let build_index files ~(index_file : Filename.t) =
  let total_number_of_files = List.length files in
  printf "Building the index...\n%!";
  let index =
    List.foldi files ~init:Equivalence_class.empty
      ~f:(fun index equivalence_counter file ->
        eprintf_progress "Processing file %d/%d\r" index
          total_number_of_files;
        List.fold (blocks_of_file file) ~init:equivalence_counter
          ~f:Equivalence_class.update)
  in
  let () = Equivalence_class.to_file index ~filename:index_file in
  printf "Saved index to %s\n" index_file;
  index
;;

let print_most_popular_classes index ~n_most_frequent_equivalences
    ~max_representatives_per_equivalence ~block_print_mode =
  let equivalences_to_print =
    List.take
      (Equivalence_class.equivalences_by_frequency index)
      n_most_frequent_equivalences
    |> Set.of_list (module Equivalence_class.Equivalence)
  in
  let how_many_printed_for_equivalence =
    Hashtbl.create (module Equivalence_class.Equivalence)
  in
  let on_block block equivalence frequency =
    match Set.mem equivalences_to_print equivalence with
    | false -> ()
    | true ->
        let current_printed =
          Hashtbl.find how_many_printed_for_equivalence equivalence
          |> Option.value ~default:0
        in
        let can_print =
          current_printed < max_representatives_per_equivalence
        in
        if can_print then (
          printf "Equivalence with %d members: \n" frequency;
          Utils.print_block block ~block_print_mode );
        Hashtbl.set how_many_printed_for_equivalence ~key:equivalence
          ~data:(if can_print then current_printed + 1 else current_printed)
  in
  let on_finish_iteration () = () in
  (on_block, on_finish_iteration)
;;

let count_equivalence_classes_of_each_size () =
  let equivalence_classes_of_each_size = Hashtbl.create (module Int) in
  let on_block _block _equivalence frequency =
    Hashtbl.update equivalence_classes_of_each_size frequency ~f:(function
      | Some x -> x + 1
      | None -> 1)
  in
  let on_finish_iteration () =
    printf
      !"(numbers of members in equivalence class, how many equivalence \
        classes there are with this many members):\n\
        %{sexp: (int, int) Hashtbl.t}\n"
      equivalence_classes_of_each_size
  in
  (on_block, on_finish_iteration)
;;

let main files ~index_file ~max_representatives_per_equivalence
    ~n_most_frequent_equivalences ~block_print_mode ~min_block_size
    ~min_equivalence_class_size =
  let index =
    if Sys.file_exists_exn index_file then (
      printf "Using cached index from %s\n%!" index_file;
      Equivalence_class.of_file ~filename:index_file )
    else build_index files ~index_file
  in
  let on_block, on_finish_iteration =
    let on_blocks, on_finish_iterations =
      List.unzip
        [ print_most_popular_classes index
            ~max_representatives_per_equivalence
            ~n_most_frequent_equivalences ~block_print_mode;
          count_equivalence_classes_of_each_size ()
        ]
    in
    let on_block block equivalence frequency =
      List.iter on_blocks ~f:(fun f -> f block equivalence frequency)
    in
    let on_finish_iteration () =
      List.iter on_finish_iterations ~f:(fun f -> f ())
    in
    (on_block, on_finish_iteration)
  in
  List.iter files ~f:(fun file ->
      List.iter (blocks_of_file file) ~f:(fun block ->
          if List.length block.body + 1 >= min_block_size then
            let equivalence =
              Equivalence_class.equivalence index block |> Option.value_exn
            in
            let frequency =
              Equivalence_class.frequency index equivalence
              |> Option.value_exn
            in
            if frequency >= min_equivalence_class_size then
              on_block block equivalence frequency));
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
