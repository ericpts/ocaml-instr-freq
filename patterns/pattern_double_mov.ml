open Instr_freq
open! Core

let f ~(block : Index.Matcher.Whole_block_predicate.block) =
  Lib.any_suffix ~block ~f:(function
    | { desc = Basic (Op Move); arg = _; res = [| move1_destination |] }
      :: { desc = Basic (Op Move); arg = [| move2_source |]; res = _ } :: _
      when move1_destination = move2_source ->
        true
    | _ -> false)
;;
