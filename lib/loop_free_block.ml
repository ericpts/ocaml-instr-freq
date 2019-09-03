open Ocamlcfg
open Core

type t = Cfg.block List.t

let create block = [ block ]

let append_successor (t : t) (successor : Cfg.block) =
  let predecessor = List.hd_exn t in
  ( match predecessor.terminator.desc with
  | Branch branch ->
      assert (
        List.exists branch ~f:(fun (_condition, label) ->
            label = successor.start) )
  | Return | Raise _ | Tailcall _ ->
      failwith "Trying to merge two incompatible blocks"
  | Switch labels ->
      assert (Array.exists labels ~f:(fun label -> label = successor.start))
  );

  successor :: t
;;

let to_list t = t

type block_print_mode =
  | As_assembly
  | As_cfg
  | Both

let print_assembly (t : t) =
  let fun_body =
    match
      List.fold t ~init:`End ~f:(fun acc block ->
          let terminator =
            match acc with
            | `End ->
                Cfg_builder.linearize_terminator block.terminator
                  ~next:Cfg_builder.labelled_insn_end
            | `Middle (previous_block_label, fun_body) ->
                Cfg_builder.linearize_terminator block.terminator
                  ~next:{ label = previous_block_label; insn = fun_body }
          in
          let fun_body =
            List.fold_right block.body ~init:terminator
              ~f:Cfg_builder.basic_to_linear
          in
          let first_insn =
            {
              Linear.desc = Linear.Llabel block.start;
              next = fun_body;
              arg = [||];
              res = [||];
              dbg = Debuginfo.none;
              live = Reg.Set.empty;
            }
          in
          `Middle (block.start, first_insn))
    with
    | `End -> failwith "Encountered empty loop_free_block!"
    | `Middle (first_block_label, fun_body) ->
        assert (first_block_label = (List.last_exn t).start);
        fun_body
  in
  let fundecl =
    {
      Linear.fun_name = "_fun_start_";
      fun_body;
      fun_fast = false;
      fun_dbg = Debuginfo.none;
      fun_spacetime_shape = None;
      fun_num_stack_slots = Caml.Array.make Proc.num_register_classes 0;
      fun_frame_required = false;
      fun_prologue_required = false;
      fun_contains_calls = false;
    }
  in
  X86_proc.reset_asm_code ();
  Emit.fundecl fundecl;
  X86_proc.generate_code
    (Some (X86_gas.generate_asm !Emitaux.output_channel))
;;

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
      printf ".L%d:\n" block.start;
      List.iter block.body ~f:(fun instruction ->
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
        block.terminator.desc
        (print_registers block.terminator.arg)
        (print_registers block.terminator.res)
        (print_live block.terminator.live)
      |> indent_string |> print_endline);

  Sexp.default_indent := init_indent
;;

let print (t : t) (block_print_mode : block_print_mode) =
  let ids_of_blocks_contained = List.map t ~f:(fun block -> block.start) in
  print_endline
    (Utils.color Green
       (sprintf
          !"Blocks %{sexp: int List.t}: {"
          (List.rev ids_of_blocks_contained)));

  ( match block_print_mode with
  | As_assembly -> print_assembly t
  | As_cfg -> print_cfg t
  | Both ->
      print_assembly t;
      print_endline (Utils.color Green (String.make 10 '='));
      print_cfg t );
  print_endline (Utils.color Green "}")
;;

let read_file ~(file : Filename.t) ~context_length =
  let rec generate_context_for_block (block : Cfg.block) cfg_builder context
      =
    match context with
    | 0 -> [| create block |]
    | _ ->
        Array.concat_map
          (Cfg.LabelSet.elements block.predecessors |> Array.of_list)
          ~f:(fun label ->
            let previous_block =
              Cfg_builder.get_block cfg_builder label |> Option.value_exn
            in
            Array.map
              (generate_context_for_block previous_block cfg_builder
                 (context - 1))
              ~f:(fun predecessor -> append_successor predecessor block))
  in
  let open Linear_format in
  let ui, _ = restore file in
  List.filter_map ui.items ~f:(fun item ->
      match item with
      | Func f ->
          let cfg_builder =
            Cfg_builder.from_linear f ~preserve_orig_labels:true
          in
          let layout = Cfg_builder.get_layout cfg_builder in
          let blocks =
            Array.concat_map (Array.of_list layout) ~f:(fun label ->
                let block =
                  Cfg_builder.get_block cfg_builder label
                  |> Option.value_exn
                in
                generate_context_for_block block cfg_builder context_length)
          in
          Some (f, blocks)
      | Data _ -> None)
;;
