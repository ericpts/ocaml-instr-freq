open Instr_freq
open Core
open Equivalence

(* This is a simple implementation, and for now considers something used if
   it appears in the [args] of an instruction, and considers something
   defined if it appears in the [res] of an instruction. *)
type t

type uses =
  | Uses of int Array.t
  | Register_not_found
[@@deriving sexp]

type def =
  | At_instruction of int
  | Not_defined_in_this_block
  | Register_not_found
[@@deriving sexp]

val create : block:Index.Matcher.Whole_block_predicate.block -> t

val find_uses :
  t -> instruction_index:int -> register:Register_equivalence.t -> uses

val find_def :
  t -> instruction_index:int -> register:Register_equivalence.t -> def
