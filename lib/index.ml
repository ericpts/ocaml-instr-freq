open Core
open Ocamlcfg
module Equivalence = Int

let get_id_or_add hashtbl key =
  match Hashtbl.find hashtbl key with
  | Some id -> id
  | None ->
      let next_id = Hashtbl.length hashtbl in
      Hashtbl.set hashtbl ~key ~data:next_id;
      next_id
;;

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
      (* CR gretay for ericpts: any particular reasons for choosing the
         defaults here? growth_allowed is true by default. The default size
         is 128, which is nice and round, unlike 100 :) *)
      for_basic =
        Hashtbl.create ~size:2_000 ~growth_allowed:true (module For_basic);
      for_terminator =
        Hashtbl.create ~size:100 ~growth_allowed:true (module For_terminator);
    }
  ;;

  let get_id_for_basic (t : t) (instruction : Cfg.basic Cfg.instruction) =
    get_id_or_add t.for_basic instruction
  ;;

  let get_id_for_terminator (t : t)
      (instruction : Cfg.terminator Cfg.instruction) =
    get_id_or_add t.for_terminator instruction
  ;;

  let get_id_for_basic_exn (t : t) = Hashtbl.find_exn t.for_basic

  let get_id_for_terminator_exn (t : t) = Hashtbl.find_exn t.for_terminator

  let inverted t =
    let for_one hashtbl =
      printf "for_one\n%!";
      Hashtbl.to_alist hashtbl
      (* CR gyorsh for ericpts: why do you need to sort? can't you use ids
         for indexes into the array directly? if you must sort, can you
         first put them in the array and then sort? it's more memory
         efficient: Array.sort is const, and List.sort is n + log n,
         although they aren't big. *)
      |> List.sort ~compare:(fun (_, e1) (_, e2) ->
             Equivalence.compare e1 e2)
      |> List.mapi ~f:(fun i (instr, e) ->
             printf "%d = %d\n" i e;
             assert (i = e);
             instr)
      |> Array.of_list
    in
    (for_one t.for_basic, for_one t.for_terminator)
  ;;
end

(* The symbolic block can be thought of as a representative for an
   equivalence class. I.e., if two block are considered equivalent, then
   they should produce the same symbolic block. *)
module Symbolic_block = struct
  module T = struct
    (* This contains the unique id of every instruction *)
    type t = Equivalence.t array

    let of_block_generic (b : Cfg.block) ~get_id_for_basic
        ~get_id_for_terminator =
      let block_length = 1 + List.length b.body in
      let ret = Array.create ~len:block_length 0 in
      List.iteri b.body ~f:(fun i basic ->
          ret.(i) <- get_id_for_basic basic);
      ret.(block_length - 1) <- get_id_for_terminator b.terminator;
      ret
    ;;

    let of_block (b : Cfg.block)
        (instruction_equivalences : Equivalence_for_instructions.t) =
      of_block_generic b
        ~get_id_for_basic:
          (Equivalence_for_instructions.get_id_for_basic
             instruction_equivalences)
        ~get_id_for_terminator:
          (Equivalence_for_instructions.get_id_for_terminator
             instruction_equivalences)
    ;;

    let of_block_exn (b : Cfg.block)
        (instruction_equivalences : Equivalence_for_instructions.t) =
      of_block_generic b
        ~get_id_for_basic:
          (Equivalence_for_instructions.get_id_for_basic_exn
             instruction_equivalences)
        ~get_id_for_terminator:
          (Equivalence_for_instructions.get_id_for_terminator_exn
             instruction_equivalences)
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
  mutable frequency : int Array.t;
}

let empty () =
  {
    instruction_equivalences = Equivalence_for_instructions.empty ();
    symbolic_block_equivalences =
      Hashtbl.create ~size:100_000 ~growth_allowed:true
        (module Symbolic_block);
    frequency = Array.create ~len:200_000 0;
  }
;;

let update t (block : Cfg.block) =
  let symbolic_block =
    Symbolic_block.of_block block t.instruction_equivalences
  in
  let equivalence =
    get_id_or_add t.symbolic_block_equivalences symbolic_block
  in
  if equivalence >= Array.length t.frequency then (
    assert (
      Hashtbl.length t.symbolic_block_equivalences
      = Array.length t.frequency + 1 );

    (* CR gyorsh for ericpts: Array.init would do it in one pass *)
    let new_frequency =
      Array.create ~len:(Array.length t.frequency * 2) 0
    in
    Array.iteri t.frequency ~f:(fun i x -> new_frequency.(i) <- x);
    for i = 0 to Array.length t.frequency - 1 do
      assert (new_frequency.(i) >= 1)
    done;
    t.frequency <- new_frequency );

  t.frequency.(equivalence) <- t.frequency.(equivalence) + 1
;;

let frequency_exn t equivalence = t.frequency.(equivalence)

let equivalence_exn t (block : Cfg.block) : Equivalence.t =
  let symbolic_block =
    Symbolic_block.of_block_exn block t.instruction_equivalences
  in
  Hashtbl.find_exn t.symbolic_block_equivalences symbolic_block
;;

let to_file t ~filename =
  (* We cannot directly marshal Core's hash tables themselves, as they
     contain closures. *)
  let as_alist =
    (* Sanity check, that we did not somehow end up with a discontinuous
       frequency array. *)
    let n_classes = Hashtbl.length t.symbolic_block_equivalences in
    for i = 0 to n_classes - 1 do
      assert (t.frequency.(i) >= 1)
    done;

    for i = n_classes to Array.length t.frequency - 1 do
      assert (t.frequency.(i) = 0)
    done;
    Array.unsafe_truncate t.frequency ~len:n_classes;

    ( Hashtbl.to_alist t.instruction_equivalences.for_basic,
      Hashtbl.to_alist t.instruction_equivalences.for_terminator,
      Hashtbl.to_alist t.symbolic_block_equivalences,
      t.frequency )
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
        frequency;
      })
;;

(* CR gyorsh for ericpts: does Hashtbl.to_alist guarantee that the order of
   pairs in the resulting list is the order of keys? also, wouldn't to_alist
   allocate a huge new list? Another option is to iterate over the hashtable
   and then use random access into the frequency array.

   Array.length symbolic_block breaks the abstraction of how symbolic blocks
   are represented.

   why do you use backticks and not records or named arguments? *)
let filter_and_sort_equivalences t ~min_block_size =
  List.zip_exn
    (Array.to_list t.frequency)
    (Hashtbl.to_alist t.symbolic_block_equivalences)
  |> List.filter_map ~f:(fun (frequency, (symbolic_block, equivalence)) ->
         if Array.length symbolic_block >= min_block_size then
           Some
             (`frequency frequency, symbolic_block, `equivalence equivalence)
         else None)
  |> List.sort ~compare:(fun (`frequency f1, _, _) (`frequency f2, _, _) ->
         -Int.compare f1 f2)
;;

let equivalences_by_frequency t ~min_block_size =
  filter_and_sort_equivalences t ~min_block_size
  |> List.map ~f:(fun (_, _, `equivalence e) -> e)
;;

let print_load t =
  sprintf "Symbolic_blocks: %d; Instructions: (basic: %d) (terminator: %d)"
    (Hashtbl.length t.symbolic_block_equivalences)
    (Hashtbl.length t.instruction_equivalences.for_basic)
    (Hashtbl.length t.instruction_equivalences.for_terminator)
;;

let print_most_frequent t ~min_block_size ~n_most_frequent_equivalences =
  let filtered =
    List.take
      (filter_and_sort_equivalences t ~min_block_size)
      n_most_frequent_equivalences
  in
  let inverted_basic, inverted_terminator =
    Equivalence_for_instructions.inverted t.instruction_equivalences
  in
  let invert_symbolic_block (s : Symbolic_block.t) =
    let block_length = Array.length s in
    let body =
      List.init (block_length - 1) ~f:(fun i -> inverted_basic.(i))
    in
    let terminator = inverted_terminator.(s.(block_length - 1)) in
    { Cfg.start = -1; body; terminator; predecessors = Cfg.LabelSet.empty }
  in
  List.iter filtered ~f:(fun (`frequency f, s, `equivalence e) ->
      printf "Equivalence class %d with frequency %d: \n" e f;
      Utils.print_block (invert_symbolic_block s) ~block_print_mode:`As_cfg)
;;
