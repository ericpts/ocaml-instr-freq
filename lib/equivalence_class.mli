open! Core
open Ocamlcfg

type remainder = int

type t

val empty : t

val update : t -> Cfg.block -> t

(* val to_alist : t -> (remainder * int) List.t
 * 
 * val representative_blocks : t -> remainder -> Cfg.block list option *)
