open Ocamlcfg

(* These statistics are ran over all of the code.

   Iteration over the blocks will stop when it returns `Stop, so the sooner
   this happens the lower the overall running time.

   The order in which the blocks are traversed is arbitrary, so your code
   should not depend on that. *)
type t = {
  on_block :
    Cfg.block ->
    equivalence:Index.Block_equivalence.t ->
    frequency:int ->
    [ `Stop | `Continue ];
  on_finish_iteration : unit -> unit;
}

val combine : t list -> t

val print_most_popular_classes :
  Index.t ->
  n_most_frequent_equivalences:int ->
  n_real_blocks_to_print:int ->
  block_print_mode:[< `As_assembly | `As_cfg ] ->
  min_block_size:int ->
  t

val count_equivalence_classes_of_each_size : unit -> t
