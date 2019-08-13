open Core
open Ocamlcfg
open Instr_freq

let print_block (block : Cfg.block) =
  printf "Block %d: {\n" block.start;
  List.iter block.body ~f:(fun instruction ->
      printf
        !"\t%{sexp:Strict_comparisons.For_cfg.basic};\n"
        instruction.desc);
  printf "}\n"
;;

let main files =
  let open Linear_format in
  let equivalence_counter =
    List.fold files ~init:Equivalence_class.empty
      ~f:(fun equivalence_counter file ->
        let linear_item_info = read file in
        Cmm.set_label linear_item_info.last_label;
        let items =
          List.filter_map linear_item_info.items ~f:(function
            | Func d ->
                d.decl
                |> Cfg_builder.from_linear ~preserve_orig_labels:false
                |> Some
            | Data _ -> None)
        in
        List.fold items ~init:equivalence_counter ~f:(fun acc cfg_builder ->
            let layout = Cfg_builder.get_layout cfg_builder in
            let blocks =
              List.map layout ~f:(fun label ->
                  Cfg_builder.get_block cfg_builder label
                  |> Option.value_exn)
            in
            (* List.iter blocks ~f:print_block; *)
            List.fold ~init:acc blocks ~f:Equivalence_class.update))
  in
  let equivalence_list = Equivalence_class.to_alist equivalence_counter in
  List.take equivalence_list 10
  |> List.iter ~f:(fun (key, data) ->
         printf "for equivalence class %d we have %d entries\n" key data;

         printf "Representative blocks: \n";

         Equivalence_class.representative_blocks equivalence_counter key
         |> Option.value_exn |> List.iter ~f:print_block;
         printf "========================================\n\n\n\n")
;;

let main_command =
  Command.basic ~summary:"Count frequency of basic blocks."
    ~readme:(fun () ->
      "Group contents of basic blocks based modulo register renaming, and \
       print the most common quotients.")
    [%map_open.Command.Let_syntax
      let files = anon (sequence ("input" %: Filename.arg_type)) in
      fun () -> main files]
;;

let () = Command.run main_command
