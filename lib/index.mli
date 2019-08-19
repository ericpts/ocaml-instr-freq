open! Core
open Ocamlcfg

(* Index is a collection of equivalence classes *)

(* Equivalence.t represents an equivalence class (not the entire relation) *)
module Equivalence : sig
  type t [@@deriving compare, hash, sexp]

  include Comparable with type t := t

  val to_int : t -> int
end

type t

val empty : unit -> t

val update : t -> Cfg.block -> unit

val equivalence_exn : t -> Cfg.block -> Equivalence.t

val frequency_exn : t -> Equivalence.t -> int

val to_file : t -> filename:Filename.t -> unit

val of_file : filename:Filename.t -> t

val equivalences_by_frequency :
  t -> min_block_size:int -> Equivalence.t list

(* CR gyorsh for ericpts: what does "load" mean here? *)
val print_load : t -> string

val print_most_frequent :
  t -> min_block_size:int -> n_most_frequent_equivalences:int -> unit
