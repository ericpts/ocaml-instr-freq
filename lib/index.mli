open! Core
open Ocamlcfg
open Equivalence

(* An index is a collection of equivalence classes *)
module T : sig
  type t
end

type t = T.t

module With_register_information : sig
  type 'a t = {
    desc : 'a;
    arg : Register_equivalence.t array;
    res : Register_equivalence.t array;
  }
  [@@deriving compare, sexp, equal]
end

(* In case we are considering applying a peephole optimization, we want to
   see how frequent a pattern is.

   A [subsequence matcher] does exactly that: receives an instruction
   pattern and matches symbolic blocks. For now, the instruction pattern is
   composed of the instruction and register equivalence classes (i.e., do
   two instructions operate on the same registers, or not?), and it only
   matches patches which are sequential (i.e., only $i1, $i2, $i3; and not
   $i1, <anything>, $i2).

   With_reg_information.t is used to enforce that two instructions operate
   on the same (or possibly different) registers.

   For example, we might be looking for patterns with redundant moves one
   after the other:

   mov r1, r2; mov r1, r2

   In this case, (arg, res) of the two instructions should contain the same
   values. Should you wish to enforce that the instructions operate on
   distinct registers, supply different integers to the (arg, res) fields.

   A [whole block matcher] lets you define your own functionality, as it
   expects a predicate which matches whole blocks. *)
module Matcher : sig
  type t

  type desc =
    | Basic of Cfg.basic
    | Terminator of Cfg.terminator
  [@@deriving sexp]

  val create_for_subsequence :
    instructions:desc With_register_information.t Array.t -> index:T.t -> t

  val create_for_whole_block :
    f:(block:desc With_register_information.t Array.t -> bool) ->
    index:T.t ->
    t
end

val empty : unit -> t

val update : ?source_file:Filename.t -> t -> Loop_free_block.t -> unit

val equivalence_exn : t -> Loop_free_block.t -> Block_equivalence.t

val frequency_exn : t -> Block_equivalence.t -> int

val to_file : t -> filename:Filename.t -> unit

val of_file : filename:Filename.t -> t

val equivalences_by_frequency :
  t ->
  min_block_size:int ->
  matcher:Matcher.t option ->
  Block_equivalence.t list

type hashtbl_load_statistics = {
  n_symbolic_blocks : int;
  n_basic_instructions : int;
  n_terminator_instructions : int;
}

val hashtbl_load_statistics : t -> hashtbl_load_statistics

val sample_file : t -> Block_equivalence.t -> Filename.t Option.t
