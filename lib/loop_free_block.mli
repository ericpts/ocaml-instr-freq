module BB = Ocamlcfg.Cfg.Basic_block

type t

val create : BB.t -> t

val append_successor : t -> BB.t -> t

val to_list : t -> BB.t List.t

type block_print_mode =
  | As_assembly
  | As_cfg
  | Both

val print : t -> block_print_mode -> unit

val read_file :
  file:string -> context_length:int -> (Linear.fundecl * t Array.t) List.t
