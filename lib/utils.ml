open Ocamlcfg
open Core

let chain_compare c1 c2 =
  match Lazy.force c1 with
  | 0 -> Lazy.force c2
  | x -> x
;;

let chain_compare_many list =
  List.fold_until list ~init:0
    ~f:(fun acc cur ->
      match acc with
      | 0 -> Continue_or_stop.Continue (Lazy.force cur)
      | x -> Continue_or_stop.Stop x)
    ~finish:Fn.id
;;

type basic_color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

let basic_color_to_int = function
  | Black -> 0
  | Red -> 1
  | Green -> 2
  | Yellow -> 3
  | Blue -> 4
  | Magenta -> 5
  | Cyan -> 6
  | White -> 7
;;

let color basic_color text =
  let number = basic_color_to_int basic_color in
  Printf.sprintf "\027[38;5;%dm%s\027[0m" number text
;;

let emit_assembly (block : Cfg.block) =
  let terminator =
    Cfg_builder.linearize_terminator block.terminator
      ~next:Cfg_builder.labelled_insn_end
  in
  let fun_body =
    List.fold_right block.body ~init:terminator
      ~f:Cfg_builder.basic_to_linear
  in
  let fundecl =
    {
      Linear.fun_name = "block_to_print";
      fun_body;
      fun_fast = false;
      fun_dbg = Debuginfo.none;
      fun_spacetime_shape = None;
    }
  in
  X86_proc.reset_asm_code ();
  Emit.fundecl fundecl;
  X86_proc.generate_code
    (Some (X86_gas.generate_asm !Emitaux.output_channel))
;;

let print_block (block : Cfg.block) ~block_print_mode =
  print_endline (color Green (sprintf "Block %d: {" block.start));

  ( match block_print_mode with
  | `As_assembly -> emit_assembly block
  | `As_cfg ->
      List.iter block.body ~f:(fun instruction ->
          printf
            !"\t%{sexp:Strict_comparisons.For_cfg.basic};\n"
            instruction.desc);

      printf
        !"\t#-%{sexp:Strict_comparisons.For_cfg.terminator}-#\n"
        block.terminator.desc );

  print_endline (color Green "}")
;;
