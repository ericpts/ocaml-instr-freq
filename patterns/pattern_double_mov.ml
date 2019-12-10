open Instr_freq
open! Core

let f ~(block : Index.Matcher.Whole_block_predicate.block) =
  Lib.any_suffix ~block ~f:(function
    | { desc = Basic (Op Move); arg = _; res = [| move1_destination |] }
      :: { desc = Basic (Op Move); arg = [| move2_source |]; res = _ } :: _
      ->
        if
          Equivalence.Register_equivalence.equal move1_destination
            move2_source
        then true
        else false
    | _ -> false)
;;
