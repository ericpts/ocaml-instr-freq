module type Equivalence = sig
  type t [@@deriving compare, hash, sexp, equal]

  val to_int : t -> int

  val of_int : int -> t
end

module Register_equivalence : Equivalence

module Block_equivalence : Equivalence

module Basic_instruction_equivalence : Equivalence

module Terminator_instruction_equivalence : Equivalence

(* Sometimes we do not care to differentiate whether the instructions are
   basics or terminators, so this is the equivalance class encompassing both
   types. Note that you should use the more specific one whenever you can. *)
module Generic_instruction_equivalence : sig
  type t

  include Equivalence with type t := t

  val of_basic : Basic_instruction_equivalence.t -> t

  val of_terminator : Terminator_instruction_equivalence.t -> t

  type unwrap =
    | Basic of Basic_instruction_equivalence.t
    | Terminator of Terminator_instruction_equivalence.t

  val unwrap : t -> unwrap
end
