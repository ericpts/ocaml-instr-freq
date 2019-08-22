open! Core
open Ocamlcfg

(* An index is a collection of equivalence classes *)
type t

module Block_equivalence : sig
  type t [@@deriving compare, hash, sexp]

  val to_int : t -> int
end

val empty : unit -> t

val update : t -> Cfg.block -> unit

val equivalence_exn : t -> Cfg.block -> Block_equivalence.t

val frequency_exn : t -> Block_equivalence.t -> int

val to_file : t -> filename:Filename.t -> unit

val of_file : filename:Filename.t -> t

val equivalences_by_frequency :
  t -> min_block_size:int -> Block_equivalence.t list

(* XCR gyorsh for ericpts: what does "load" mean here? *)
val print_hashtbl_load_statistics : t -> string

val print_most_frequent :
  t -> min_block_size:int -> n_most_frequent_equivalences:int -> unit
