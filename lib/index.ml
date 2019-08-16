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
        Types.From_cfg.compare_basic_instruction
      ;;

      let sexp_of_t (t : t) = Types.From_cfg.sexp_of_basic t.desc

      let hash (t : t) = Types.From_cfg.hash_basic t.desc
    end

    include T
    include Comparator.Make (T)
  end

  module For_terminator = struct
    module T = struct
      type t = Cfg.terminator Cfg.instruction

      let compare = Types.From_cfg.compare_terminator_instruction

      let sexp_of_t (t : t) = Types.From_cfg.sexp_of_terminator t.desc

      let hash (t : t) = Types.From_cfg.hash_terminator t.desc
    end

    include T
    include Comparator.Make (T)
  end

  type t = {
    for_basic : (Cfg.basic Cfg.instruction, Equivalence.t) Hashtbl.t;
    for_terminator :
      (Cfg.terminator Cfg.instruction, Equivalence.t) Hashtbl.t;
  }

  let empty () =
    {
      for_basic = Hashtbl.create ~size:100_000 (module For_basic);
      for_terminator = Hashtbl.create ~size:100_000 (module For_terminator);
    }
  ;;

  let next_id t =
    Hashtbl.length t.for_basic + Hashtbl.length t.for_terminator + 1
  ;;

  let get_id_for_basic (t : t) (instruction : Cfg.basic Cfg.instruction) =
    match Hashtbl.find t.for_basic instruction with
    | Some id -> id
    | None ->
        let id = next_id t in
        Hashtbl.set t.for_basic ~key:instruction ~data:id;
        id
  ;;

  let get_id_for_terminator (t : t)
      (instruction : Cfg.terminator Cfg.instruction) =
    match Hashtbl.find t.for_terminator instruction with
    | Some id -> id
    | None ->
        let id = next_id t in
        Hashtbl.set t.for_terminator ~key:instruction ~data:id;
        id
  ;;

  let get_id_for_basic_opt (t : t) = Hashtbl.find t.for_basic

  let get_id_for_terminator_opt (t : t) = Hashtbl.find t.for_terminator
end

module Equivalence_for_blocks = struct
  module T = struct
    type t = Cfg.block

    let compare = Types.From_cfg.compare_block

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
      let ids_for_body_reversed =
        List.fold b.body ~init:[] ~f:(fun acc cur ->
            let id =
              Equivalence_for_instructions.get_id_for_basic
                instruction_equivalences cur
            in
            id :: acc)
      in
      let id_for_terminator =
        Equivalence_for_instructions.get_id_for_terminator
          instruction_equivalences b.terminator
      in
      id_for_terminator :: ids_for_body_reversed |> Array.of_list_rev
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

    let hash t =
      Array.fold t ~init:(Hash.alloc ()) ~f:(fun acc cur ->
          Int.hash_fold_t acc cur)
      |> Hash.get_hash_value
    ;;
  end

  include T
  include Comparator.Make (T)
end

type t = {
  instruction_equivalences : Equivalence_for_instructions.t;
  symbolic_block_equivalences : (Symbolic_block.t, Equivalence.t) Hashtbl.t;
  frequency : (Equivalence.t, int) Hashtbl.t; (* TODO: change to array.*)
}

let empty () =
  {
    instruction_equivalences = Equivalence_for_instructions.empty ();
    symbolic_block_equivalences =
      Hashtbl.create ~size:100_000 (module Symbolic_block);
    frequency = Hashtbl.create ~size:100_000 (module Int);
  }
;;

let update t (block : Cfg.block) =
  let symbolic_block =
    Symbolic_block.of_block block t.instruction_equivalences
  in
  Hashtbl.update t.symbolic_block_equivalences symbolic_block ~f:(function
    | Some x -> x
    | None -> Hashtbl.length t.symbolic_block_equivalences + 1);
  let equivalence =
    Hashtbl.find_exn t.symbolic_block_equivalences symbolic_block
  in
  Hashtbl.update t.frequency equivalence ~f:(function
    | Some f -> f + 1
    | None -> 1)
;;

let frequency t = Hashtbl.find t.frequency

let equivalence t (block : Cfg.block) : Equivalence.t option =
  let open Option.Let_syntax in
  let%bind symbolic_block =
    Symbolic_block.of_block_opt block t.instruction_equivalences
  in
  Hashtbl.find t.symbolic_block_equivalences symbolic_block
;;

let to_file t ~filename =
  let as_alist =
    ( Hashtbl.to_alist t.instruction_equivalences.for_basic,
      Hashtbl.to_alist t.instruction_equivalences.for_terminator,
      Hashtbl.to_alist t.symbolic_block_equivalences,
      Hashtbl.to_alist t.frequency )
  in
  Out_channel.with_file ~binary:true filename ~f:(fun out_channel ->
      Marshal.to_channel out_channel as_alist [])
;;

let of_file ~filename =
  In_channel.with_file ~binary:true filename ~f:(fun inc ->
      let for_basic, for_terminator, symbolic_block_equivalences, frequency
          =
        Marshal.from_channel inc
      in
      {
        instruction_equivalences =
          {
            Equivalence_for_instructions.for_basic =
              for_basic
              |> Hashtbl.of_alist_exn
                   (module Equivalence_for_instructions.For_basic);
            Equivalence_for_instructions.for_terminator =
              for_terminator
              |> Hashtbl.of_alist_exn
                   (module Equivalence_for_instructions.For_terminator);
          };
        symbolic_block_equivalences =
          symbolic_block_equivalences
          |> Hashtbl.of_alist_exn (module Symbolic_block);
        frequency = frequency |> Hashtbl.of_alist_exn (module Int);
      })
;;

let equivalences_by_frequency t =
  Hashtbl.to_alist t.frequency
  |> List.sort ~compare:(fun (_e1, f1) (_e2, f2) -> -Int.compare f1 f2)
  |> List.map ~f:fst
;;

let print_load t =
  sprintf "Symbolic_blocks: %d; Instructions: %d %d"
    (Hashtbl.length t.symbolic_block_equivalences)
    (Hashtbl.length t.instruction_equivalences.for_terminator)
    (Hashtbl.length t.instruction_equivalences.for_basic)
;;
