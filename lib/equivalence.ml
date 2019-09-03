open Core

module type Equivalence = sig
  type t [@@deriving compare, hash, sexp, equal]

  val to_int : t -> int

  val of_int : int -> t
end

module Register_equivalence = Int
module Block_equivalence = Int
module Basic_instruction_equivalence = Int
module Terminator_instruction_equivalence = Int

module Generic_instruction_equivalence = struct
  include Int

  (* Positive values indicate basic instructions; negative values indicate
     terminator instructions *)

  let of_basic (basic : Basic_instruction_equivalence.t) =
    assert (basic >= 0);
    basic + 1
  ;;

  let of_terminator (terminator : Terminator_instruction_equivalence.t) =
    assert (terminator >= 0);
    -(terminator + 1)
  ;;

  let unwrap t =
    match t with
    | p when p > 0 -> `Basic (Basic_instruction_equivalence.of_int (p - 1))
    | n when n < 0 ->
        `Terminator (Terminator_instruction_equivalence.of_int (-n - 1))
    | x ->
        failwithf
          "Got unexpected generic_instruction_equivalence class equal to %d"
          x ()
  ;;
end
