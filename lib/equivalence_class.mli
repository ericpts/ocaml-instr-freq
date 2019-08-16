open! Core
open Ocamlcfg

module Equivalence : sig
  type t [@@deriving compare, hash, sexp]

  include Comparable with type t := t
end

type t

val empty : unit -> t

val update : t -> Cfg.block -> unit

val equivalence : t -> Cfg.block -> Equivalence.t option

val frequency : t -> Equivalence.t -> int option

val to_file : t -> filename:Filename.t -> unit

val of_file : filename:Filename.t -> t

val equivalences_by_frequency : t -> Equivalence.t list
