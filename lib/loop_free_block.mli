open Ocamlcfg

type t

val create : Cfg.block -> t

val append_successor : t -> Cfg.block -> t

val to_list : t -> Cfg.block List.t

type block_print_mode =
  | As_assembly
  | As_cfg
  | Both

val print : t -> block_print_mode -> unit
