open Instr_freq
open Core

let any_suffix ~(block : Index.Matcher.Whole_block_predicate.block) ~f =
  let rec impl list =
    match list with
    | [] -> false
    | _ :: tail -> if f list then true else impl tail
  in
  impl (Array.to_list block)
;;
