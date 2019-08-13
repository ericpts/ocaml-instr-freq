open Ocamlcfg
open Core

let chain_compare c1 c2 =
  match c1 with
  | 0 -> c2
  | x -> x
;;

let print_block (block : Cfg.block) =
  printf "Block %d: {\n" block.start;
  List.iter block.body ~f:(fun instruction ->
      printf
        !"\t%{sexp:Strict_comparisons.For_cfg.basic};\n"
        instruction.desc);

  printf
    !"\t#-%{sexp:Strict_comparisons.For_cfg.terminator}-#\n"
    block.terminator.desc;

  printf "}\n"
;;
