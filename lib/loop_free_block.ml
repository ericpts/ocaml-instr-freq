open Core
module Cfg = Ocamlcfg.Cfg
module BB = Cfg.Basic_block
module CL = Ocamlcfg.Cfg_with_layout

type t = BB.t List.t

let create block = [block]

let append_successor (t : t) (successor : BB.t) =
  let predecessor = List.hd_exn t in
  ( match (BB.terminator predecessor).desc with
  | Branch branch ->
      assert (
        List.exists branch ~f:(fun (_condition, label) ->
            label = BB.start successor) )
  | Return | Raise _ | Tailcall _ ->
      failwith "Trying to merge two incompatible blocks"
  | Switch labels ->
      assert (
        Array.exists labels ~f:(fun label -> label = BB.start successor) ) );

  successor :: t
;;

let to_list t = t

type block_print_mode =
  | As_assembly
  | As_cfg
  | Both

let print_cfg (t : t) =
  let init_indent = !Sexp.default_indent in
  Sexp.default_indent := 4;
  let indent_string s =
    String.split_lines s
    |> List.map ~f:(fun s -> "\t" ^ s)
    |> String.concat ~sep:"\n"
  in
  List.iter (List.rev t) ~f:(fun block ->
      let print_registers registers =
        Array.map registers
          ~f:
            (Types.Modulo_register_renaming.symbolize_register
               ~include_register_number:true)
      in
      let print_live live =
        print_registers
          ( Reg.Set.to_seq live
          |> Seq.fold_left (fun x a -> a :: x) []
          |> Array.of_list )
      in
      printf ".L%d:\n" (BB.start block);
      List.iter (BB.body block) ~f:(fun instruction ->
          sprintf
            !"%{sexp:Types.From_cfg.basic}: Arg%{sexp:string array} \
              Res%{sexp:string array} Live%{sexp: string array}\n"
            instruction.desc
            (print_registers instruction.arg)
            (print_registers instruction.res)
            (print_live instruction.live)
          |> indent_string |> print_endline);

      sprintf
        !"#-%{sexp:Types.From_cfg.terminator}-# Arg%{sexp:string array} \
          Res%{sexp:string array} Live%{sexp: string array} \n"
        (BB.terminator block).desc
        (print_registers (BB.terminator block).arg)
        (print_registers (BB.terminator block).res)
        (print_live (BB.terminator block).live)
      |> indent_string |> print_endline);

  Sexp.default_indent := init_indent
;;

let print (t : t) (block_print_mode : block_print_mode) =
  let ids_of_blocks_contained =
    List.map t ~f:(fun block -> BB.start block)
  in
  print_endline
    (Utils.color Green
       (sprintf
          !"Blocks %{sexp: int List.t}: {"
          (List.rev ids_of_blocks_contained)));

  ( match block_print_mode with
  | As_assembly -> (
      match t with
      | [] -> failwith "Encountered empty loop_free_block!"
      | _ -> Ocamlcfg.Util.print_assembly (List.rev t) )
  | As_cfg -> print_cfg t
  | Both ->
      Ocamlcfg.Util.print_assembly (List.rev t);
      print_endline (Utils.color Green (String.make 10 '='));
      print_cfg t );
  print_endline (Utils.color Green "}")
;;

let read_file ~(file : Filename.t) ~context_length =
  let rec generate_context_for_block (block : BB.t) cfg context =
    match context with
    | 0 -> [| create block |]
    | _ ->
        Array.concat_map
          (Cfg.predecessors block |> Array.of_list)
          ~f:(fun label ->
            let previous_block =
              Cfg.get_block cfg label |> Option.value_exn
            in
            Array.map
              (generate_context_for_block previous_block cfg (context - 1))
              ~f:(fun predecessor -> append_successor predecessor block))
  in
  let open Linear_format in
  let ui, _ = restore file in
  List.filter_map ui.items ~f:(fun item ->
      match item with
      | Func f ->
          let cl = CL.of_linear f ~preserve_orig_labels:true in
          let cfg = CL.cfg cl in
          let layout = CL.layout cl in
          let blocks =
            Array.concat_map (Array.of_list layout) ~f:(fun label ->
                let block = Cfg.get_block cfg label |> Option.value_exn in
                generate_context_for_block block cfg context_length)
          in
          Some (f, blocks)
      | Data _ -> None)
;;
