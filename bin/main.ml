open Core
open Ocamlcfg
open Instr_freq

let block_size_for_equivalence_class equivalence_counter equivalence_class =
  Equivalence_class.representative_blocks equivalence_counter
    equivalence_class
  |> Option.value_exn |> List.hd_exn
  |> fun block ->
  1 + List.length block.Cfg.body
;;

let main files ~representatives_per_equivalence_class ~most_frequent_classes
    ~block_print_mode ~min_block_size ~min_equivalence_class_size =
  let open Linear_format in
  let equivalence_counter =
    List.fold files
      ~init:(Equivalence_class.empty ~representatives_per_equivalence_class)
      ~f:(fun equivalence_counter file ->
        let items = restore file in
        let function_blocks =
          List.filter_map items ~f:(fun item ->
              restore_item item;
              match item with
              | Func d ->
                  d.decl
                  |> Cfg_builder.from_linear ~preserve_orig_labels:false
                  |> Some
              | Data _ -> None)
        in
        List.fold function_blocks ~init:equivalence_counter
          ~f:(fun acc cfg_builder ->
            let layout = Cfg_builder.get_layout cfg_builder in
            let blocks =
              List.map layout ~f:(fun label ->
                  Cfg_builder.get_block cfg_builder label
                  |> Option.value_exn)
            in
            (* List.iter blocks ~f:print_block; *)
            List.fold ~init:acc blocks ~f:Equivalence_class.update))
  in
  let equivalence_list =
    Equivalence_class.to_alist equivalence_counter
    |> List.filter ~f:(fun (equivalence_class, _number_of_members) ->
           block_size_for_equivalence_class equivalence_counter
             equivalence_class
           >= min_block_size)
    |> List.filter ~f:(fun (_equivalence_class, number_of_members) ->
           number_of_members >= min_equivalence_class_size)
  in
  List.take equivalence_list most_frequent_classes
  |> List.iter ~f:(fun (equivalence_class, number_of_members) ->
         printf "for equivalence class %d we have %d entries\n"
           equivalence_class number_of_members;
         printf "Representative blocks: \n";
         Equivalence_class.representative_blocks equivalence_counter
           equivalence_class
         |> Option.value_exn
         |> List.iter ~f:(Utils.print_block ~block_print_mode);
         printf "\n========================================\n");

  let equivalence_classes_of_each_size = Hashtbl.create (module Int) in
  List.iter equivalence_list
    ~f:(fun (_equivalence_class, number_of_members) ->
      Hashtbl.update equivalence_classes_of_each_size number_of_members
        ~f:(function
        | Some x -> x + 1
        | None -> 1));
  printf
    !"(numbers of members in equivalence class, how many equivalence \
      classes there are with this many members):\n\
      %{sexp: (int, int) Hashtbl.t}\n"
    equivalence_classes_of_each_size
;;

let main_command =
  Command.basic ~summary:"Count frequency of basic blocks."
    ~readme:(fun () ->
      "Group contents of basic blocks based on equivalence classes, and \
       print the most common classes.")
    [%map_open.Command.Let_syntax
      let files = anon (sequence ("input" %: Filename.arg_type))
      and representatives_per_equivalence_class =
        flag "-representatives-per-equivalence-class"
          (optional_with_default 5 int)
          ~doc:"n Save representative blocks for each equivalence class"
      and most_frequent_classes =
        flag "-most-frequent-classes"
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
      in
      let block_print_mode =
        match print_blocks_as_assembly with
        | true -> `As_assembly
        | false -> `As_cfg
      in
      fun () ->
        main files ~representatives_per_equivalence_class
          ~most_frequent_classes ~block_print_mode ~min_block_size
          ~min_equivalence_class_size]
;;

let () = Command.run main_command
