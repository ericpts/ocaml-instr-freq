open Core
open Ocamlcfg

let max_representatives_per_equivalence_class = 10

type remainder = int

module Equivalence_for_blocks = struct
  module T = struct
    type t = Cfg.block

    let compare = Equivalence_comparisons.For_cfg.compare_block

    let sexp_of_t _t = Sexp.Atom "<block>"
  end

  include T
  include Comparator.Make (T)
end

type t = {
  equivalence_classes :
    (Cfg.block, remainder, Equivalence_for_blocks.comparator_witness) Map.t;
  frequency : (remainder, int, Int.comparator_witness) Map.t;
  representatives :
    (remainder, Cfg.block list, Int.comparator_witness) Map.t;
}

let empty =
  {
    equivalence_classes = Map.empty (module Equivalence_for_blocks);
    frequency = Map.empty (module Int);
    representatives = Map.empty (module Int);
  }
;;

let update t block =
  let new_equivalence_classes =
    Map.update t.equivalence_classes block ~f:(function
      | Some equivalence_class -> equivalence_class
      | None -> Map.length t.equivalence_classes + 1)
  in
  let equivalence_class = Map.find_exn new_equivalence_classes block in
  {
    equivalence_classes = new_equivalence_classes;
    frequency =
      Map.update t.frequency equivalence_class ~f:(function
        | Some f -> f + 1
        | None -> 1);
    representatives =
      Map.update t.representatives equivalence_class ~f:(function
        | Some list ->
            if List.length list < max_representatives_per_equivalence_class
            then block :: list
            else list
        | None -> [ block ]);
  }
;;

let to_alist t =
  Map.to_alist t.frequency
  |> List.sort ~compare:(fun (_r1, f1) (_r2, f2) -> -Int.compare f1 f2)
;;

let representative_blocks t remainder = Map.find t.representatives remainder
