open Instr_freq
open! Core

let f ~(block : Index.Matcher.Whole_block_predicate.block) =
  Lib.any_suffix ~block ~f:(function
    | { desc = Basic (Op (Types.From_cfg.Intop Types.From_mach.Iadd));
        arg = _;
        res = _
      }
      :: _ ->
        true
    | _ -> false)
;;
