open Core
open Ocamlcfg
module Equivalence = Int

module Equivalence_for_instructions = struct
  module For_basic = struct
    module T = struct
      type t = Cfg.basic Cfg.instruction

      let compare =
        (* CR estavarache: Maybe there should be a more thorough register
           comparison?

           In the sense that perhaps two blocks are not equivalent in the
           register-renaming sense, however if we hash the instructions
           one-by-one, then they would become equivalent.

           One solution to this would be to first do register-renaming, and
           then only after that to hash the instructions. *)
        Equivalence_comparisons.For_cfg.compare_basic_instruction
      ;;

      let sexp_of_t (t : t) =
        Strict_comparisons.For_cfg.sexp_of_basic t.desc
      ;;
    end

    include T
    include Comparator.Make (T)
  end

  module For_terminator = struct
    module T = struct
      type t = Cfg.terminator Cfg.instruction

      let compare =
        Equivalence_comparisons.For_cfg.compare_terminator_instruction
      ;;

      let sexp_of_t (t : t) =
        Strict_comparisons.For_cfg.sexp_of_terminator t.desc
      ;;
    end

    include T
    include Comparator.Make (T)
  end

  type t = {
    for_basic :
      ( Cfg.basic Cfg.instruction,
        Equivalence.t,
        For_basic.comparator_witness )
      Map.t;
    for_terminator :
      ( Cfg.terminator Cfg.instruction,
        Equivalence.t,
        For_terminator.comparator_witness )
      Map.t;
  }

  let empty =
    {
      for_basic = Map.empty (module For_basic);
      for_terminator = Map.empty (module For_terminator);
    }
  ;;

  let next_id t = Map.length t.for_basic + Map.length t.for_terminator + 1

  let get_id_for_basic (t : t) (instruction : Cfg.basic Cfg.instruction) =
    match Map.find t.for_basic instruction with
    | Some id -> (t, id)
    | None ->
        let id = next_id t in
        ( {
            t with
            for_basic = Map.set t.for_basic ~key:instruction ~data:id;
          },
          id )
  ;;

  let get_id_for_terminator (t : t)
      (instruction : Cfg.terminator Cfg.instruction) =
    match Map.find t.for_terminator instruction with
    | Some id -> (t, id)
    | None ->
        let id = next_id t in
        ( {
            t with
            for_terminator =
              Map.set t.for_terminator ~key:instruction ~data:id;
          },
          id )
  ;;

  let get_id_for_basic_opt (t : t) = Map.find t.for_basic

  let get_id_for_terminator_opt (t : t) = Map.find t.for_terminator
end

module Equivalence_for_blocks = struct
  module T = struct
    type t = Cfg.block

    let compare = Equivalence_comparisons.For_cfg.compare_block

    let sexp_of_t _t = Sexp.Atom "<block>"
  end

  include T
  include Comparator.Make (T)
end

(* The symbolic block can be thought of as a representative for an
   equivalence class. I.e., if two block are considered equivalent, then
   they should produce the same symbolic block. *)
module Symbolic_block = struct
  module T = struct
    (* This contains the unique id of every instruction *)
    type t = Equivalence.t array

    let of_block (b : Cfg.block)
        (instruction_equivalences : Equivalence_for_instructions.t) =
      let equivalences, ids_for_body_reversed =
        List.fold b.body ~init:(instruction_equivalences, [])
          ~f:(fun (iec, list) cur ->
            let iec, id =
              Equivalence_for_instructions.get_id_for_basic iec cur
            in
            (iec, id :: list))
      in
      let equivalences, id_for_terminator =
        Equivalence_for_instructions.get_id_for_terminator equivalences
          b.terminator
      in
      ( equivalences,
        id_for_terminator :: ids_for_body_reversed |> Array.of_list_rev )
    ;;

    let of_block_opt (b : Cfg.block)
        (instruction_equivalences : Equivalence_for_instructions.t) =
      let open Option.Let_syntax in
      let%bind for_body_rev =
        List.map b.body
          ~f:
            (Equivalence_for_instructions.get_id_for_basic_opt
               instruction_equivalences)
        |> List.fold ~init:(Some []) ~f:(fun acc cur ->
               let%bind acc = acc in
               let%map cur = cur in
               cur :: acc)
      in
      let%map for_terminator =
        Equivalence_for_instructions.get_id_for_terminator_opt
          instruction_equivalences b.terminator
      in
      for_terminator :: for_body_rev |> Array.of_list_rev
    ;;

    let compare = [%compare: int array]

    let sexp_of_t = [%sexp_of: int array]
  end

  include T
  include Comparator.Make (T)
end

type t = {
  instruction_equivalences : Equivalence_for_instructions.t;
  symbolic_block_equivalences :
    ( Symbolic_block.t,
      Equivalence.t,
      Symbolic_block.comparator_witness )
    Map.t;
  frequency : (Equivalence.t, int, Int.comparator_witness) Map.t;
}

let empty =
  {
    instruction_equivalences = Equivalence_for_instructions.empty;
    symbolic_block_equivalences = Map.empty (module Symbolic_block);
    frequency = Map.empty (module Int);
  }
;;

let update t (block : Cfg.block) =
  let instruction_equivalences, symbolic_block =
    Symbolic_block.of_block block t.instruction_equivalences
  in
  let symbolic_block_equivalences =
    Map.update t.symbolic_block_equivalences symbolic_block ~f:(function
      | Some x -> x
      | None -> Map.length t.symbolic_block_equivalences + 1)
  in
  let equivalence =
    Map.find_exn symbolic_block_equivalences symbolic_block
  in
  {
    instruction_equivalences;
    symbolic_block_equivalences;
    frequency =
      Map.update t.frequency equivalence ~f:(function
        | Some f -> f + 1
        | None -> 1);
  }
;;

let frequency t = Map.find t.frequency

let equivalence t (block : Cfg.block) : Equivalence.t option =
  let open Option.Let_syntax in
  let%bind symbolic_block =
    Symbolic_block.of_block_opt block t.instruction_equivalences
  in
  Map.find t.symbolic_block_equivalences symbolic_block
;;

let to_file t ~filename =
  Out_channel.with_file ~binary:true filename ~f:(fun out_channel ->
      Marshal.to_channel out_channel t [ Marshal.Closures ])
;;

let of_file ~filename =
  In_channel.with_file ~binary:true filename ~f:Marshal.from_channel
;;

let equivalences_by_frequency t =
  Map.to_alist t.frequency
  |> List.sort ~compare:(fun (_e1, f1) (_e2, f2) -> -Int.compare f1 f2)
  |> List.map ~f:fst
;;
