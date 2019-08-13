open Core
open Ocamlcfg
open Instr_freq

let print_block (block : Cfg.block) =
  printf "Block %d: {\n" block.start;
  List.iter block.body ~f:(fun instruction ->
      printf
        !"\t%{sexp:Compare_functions.Default_comparisons.For_cfg.basic};\n"
        instruction.desc);
  printf "}\n"
;;

let main file =
  let open Linear_format in
  let linear_item_info = read file in
  let items =
    List.filter_map linear_item_info.items ~f:(function
      | Func d ->
          d.decl
          |> Cfg_builder.from_linear ~preserve_orig_labels:false
          |> Some
      | Data _ -> None)
  in
  let equivalence_counter =
    List.fold items ~init:Equivalence_class.empty ~f:(fun acc cfg_builder ->
        let layout = Cfg_builder.get_layout cfg_builder in
        let blocks =
          List.map layout ~f:(fun label ->
              Cfg_builder.get_block cfg_builder label |> Option.value_exn)
        in
        List.fold ~init:acc blocks ~f:Equivalence_class.update)
  in
  let equivalence_counter =
    Equivalence_class.to_alist equivalence_counter
  in
  List.take equivalence_counter 10
  |> List.iter ~f:(fun (key, data) ->
         printf "for equivalence class %d we have %d entries\n" key data)
;;

let main_command =
  Command.basic ~summary:"Count frequency of basic blocks."
    ~readme:(fun () ->
      "Group contents of basic blocks based modulo register renaming, and \
       print the most common quotients.")
    [%map_open.Command.Let_syntax
      let file = anon ("input" %: Filename.arg_type) in
      fun () -> main file]
;;

let () = Command.run main_command
