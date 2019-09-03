open Equivalence
open Core

(* These statistics are ran over all of the code.

   Iteration over the blocks will stop when it returns `Stop, so the sooner
   this happens the lower the overall running time.

   The order in which the blocks are traversed is arbitrary, so your code
   should not depend on that. *)
type t = {
  on_block :
    Loop_free_block.t ->
    file:string ->
    equivalence:Block_equivalence.t ->
    frequency:int ->
    fundecl:Linear.fundecl ->
    [ `Stop | `Continue ];
  on_finish_iteration : unit -> unit;
  (* These files might contain relevant information, (i.e., the blocks that
     we are looking for, and that we want to print). It is recommended to
     first iterate over these files, and then everything else, in order to
     speed up the execution. *)
  hint_files : String.Set.t;
}

val combine : t list -> t

val print_most_popular_classes :
  Index.t ->
  n_most_frequent_equivalences:int ->
  n_real_blocks_to_print:int ->
  block_print_mode:Loop_free_block.block_print_mode ->
  min_block_size:int ->
  matcher:Index.Matcher.t option ->
  t

val count_blocks_matching :
  Index.t -> min_block_size:int -> matcher:Index.Matcher.t option -> t
