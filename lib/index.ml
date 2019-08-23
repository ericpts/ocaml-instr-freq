open Core
open Ocamlcfg

module type Equivalence = sig
  type t [@@deriving compare, hash, sexp]

  val to_int : t -> int

  val of_int : int -> t
end

module Block_equivalence : sig
  type t

  include Equivalence with type t := t
end =
  Int

module Basic_instruction_equivalence : sig
  type t

  include Equivalence with type t := t
end =
  Int

module Terminator_instruction_equivalence : sig
  type t

  include Equivalence with type t := t
end =
  Int

module Register_equivalence : sig
  type t

  include Equivalence with type t := t
end =
  Int

let get_id_or_add ~equivalence_of_int hashtbl key =
  match Hashtbl.find hashtbl key with
  | Some id -> id
  | None ->
      let next_id = Hashtbl.length hashtbl |> equivalence_of_int in
      Hashtbl.set hashtbl ~key ~data:next_id;
      next_id
;;

module Instruction_index = struct
  module For_basic = struct
    module T = struct
      type t = Cfg.basic Cfg.instruction

      let compare = Types.From_cfg.compare_basic_instruction

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
    for_basic :
      (Cfg.basic Cfg.instruction, Basic_instruction_equivalence.t) Hashtbl.t;
    for_terminator :
      ( Cfg.terminator Cfg.instruction,
        Terminator_instruction_equivalence.t )
      Hashtbl.t;
  }

  let empty () =
    {
      (* XCR gretay for ericpts: any particular reasons for choosing the
         defaults here? growth_allowed is true by default. The default size
         is 128, which is nice and round, unlike 100 :)

         ericpts: Empirically, this is how many values I found while playing
         around with the code, so I was hoping to avoid some allocations and
         copeies; however I think it does not make a big difference, so I'll
         just stick to the clear version. *)
      for_basic = Hashtbl.create (module For_basic);
      for_terminator = Hashtbl.create (module For_terminator);
    }
  ;;

  let get_id_for_basic (t : t) (instruction : Cfg.basic Cfg.instruction) =
    get_id_or_add ~equivalence_of_int:Basic_instruction_equivalence.of_int
      t.for_basic instruction
  ;;

  let get_id_for_terminator
      (t : t) (instruction : Cfg.terminator Cfg.instruction) =
    get_id_or_add
      ~equivalence_of_int:Terminator_instruction_equivalence.of_int
      t.for_terminator instruction
  ;;

  let get_id_for_basic_exn (t : t) = Hashtbl.find_exn t.for_basic

  let get_id_for_terminator_exn (t : t) = Hashtbl.find_exn t.for_terminator

  let inverted t =
    let for_one hashtbl ~equivalence_to_int =
      (* XCR gyorsh for ericpts: why do you need to sort? can't you use ids
         for indexes into the array directly? if you must sort, can you
         first put them in the array and then sort? it's more memory
         efficient: Array.sort is const, and List.sort is n + log n,
         although they aren't big. *)
      let arr = Array.create ~len:(Hashtbl.length hashtbl) None in
      Hashtbl.iteri hashtbl ~f:(fun ~key:instruction ~data:equivalence ->
          arr.(equivalence |> equivalence_to_int) <- Some instruction);
      Array.mapi arr ~f:(fun equivalence instruction_opt ->
          match instruction_opt with
          | Some i -> i
          | None ->
              failwithf
                "Could not find any instructions for equivalence class %d"
                equivalence ())
    in
    ( for_one t.for_basic
        ~equivalence_to_int:Basic_instruction_equivalence.to_int,
      for_one t.for_terminator
        ~equivalence_to_int:Terminator_instruction_equivalence.to_int )
  ;;
end

(* The symbolic block can be thought of as a representative for an
   equivalence class. I.e., if two block are considered equivalent, then
   they should produce the same symbolic block. *)
module Symbolic_block = struct
  module T = struct
    (* This contains the unique id of every instruction *)
    type t = {
      basics : Basic_instruction_equivalence.t array;
      terminator : Terminator_instruction_equivalence.t;
      (* Contains the registers for every instruction, including the
         terminator. *)
      registers : Register_equivalence.t array array;
    }
    [@@deriving compare, sexp_of]

    let of_block_generic
        (b : Cfg.block) ~get_id_for_basic ~get_id_for_terminator =
      let body_length = List.length b.body in
      let basics =
        Array.create ~len:body_length
          (Basic_instruction_equivalence.of_int 0)
      in
      List.iteri b.body ~f:(fun i basic ->
          basics.(i) <- get_id_for_basic basic);

      let register_index = Hashtbl.create (module String) in
      let symbolize_registers_of instr =
        Array.map
          (Array.concat [ instr.Cfg.arg; instr.res ])
          ~f:(fun reg ->
            get_id_or_add ~equivalence_of_int:Register_equivalence.of_int
              register_index
              (Types.Modulo_register_renaming.symbolize_register reg
                 ~include_register_number:true))
      in
      let registers = Array.create ~len:(body_length + 1) [||] in
      List.iteri b.body ~f:(fun i basic ->
          registers.(i) <- symbolize_registers_of basic);
      registers.(body_length) <- symbolize_registers_of b.terminator;
      { basics; terminator = get_id_for_terminator b.terminator; registers }
    ;;

    let of_block (b : Cfg.block) (instruction_index : Instruction_index.t) =
      of_block_generic b
        ~get_id_for_basic:
          (Instruction_index.get_id_for_basic instruction_index)
        ~get_id_for_terminator:
          (Instruction_index.get_id_for_terminator instruction_index)
    ;;

    let of_block_exn
        (b : Cfg.block) (instruction_index : Instruction_index.t) =
      of_block_generic b
        ~get_id_for_basic:
          (Instruction_index.get_id_for_basic_exn instruction_index)
        ~get_id_for_terminator:
          (Instruction_index.get_id_for_terminator_exn instruction_index)
    ;;

    let hash (t : t) =
      let state =
        Array.fold t.basics ~init:(Hash.alloc ()) ~f:(fun acc cur ->
            Int.hash_fold_t acc (cur |> Basic_instruction_equivalence.to_int))
      in
      let state =
        Int.hash_fold_t state
          (t.terminator |> Terminator_instruction_equivalence.to_int)
      in
      let state =
        Array.fold ~init:state t.registers ~f:(fun state arr ->
            Array.fold arr ~init:state ~f:(fun state x ->
                Int.hash_fold_t state (x |> Register_equivalence.to_int)))
      in
      Hash.get_hash_value state
    ;;

    let length (t : t) = 1 + Array.length t.basics

    let invert (t : t) ~inverted_basic ~inverted_terminator =
      let block_length = Array.length t.basics in
      let body =
        List.init (block_length - 1) ~f:(fun i ->
            inverted_basic.(t.basics.(i)
                            |> Basic_instruction_equivalence.to_int))
      in
      let terminator =
        inverted_terminator.(t.terminator
                             |> Terminator_instruction_equivalence.to_int)
      in
      {
        Cfg.start = -1;
        body;
        terminator;
        predecessors = Cfg.LabelSet.empty;
      }
    ;;
  end

  include T
  include Comparator.Make (T)
end

type t = {
  instruction_index : Instruction_index.t;
  symbolic_block_index : (Symbolic_block.t, Block_equivalence.t) Hashtbl.t;
  mutable frequency : int Array.t;
}

let empty () =
  {
    instruction_index = Instruction_index.empty ();
    symbolic_block_index = Hashtbl.create (module Symbolic_block);
    frequency = Array.create ~len:200_000 0;
  }
;;

let update t (block : Cfg.block) =
  let symbolic_block = Symbolic_block.of_block block t.instruction_index in
  let equivalence =
    get_id_or_add t.symbolic_block_index symbolic_block
      ~equivalence_of_int:Block_equivalence.of_int
    |> Block_equivalence.to_int
  in
  if equivalence >= Array.length t.frequency then (
    assert (
      Hashtbl.length t.symbolic_block_index = Array.length t.frequency + 1
    );

    (* XCR gyorsh for ericpts: Array.init would do it in one pass *)
    let new_frequency =
      let n = Array.length t.frequency in
      for i = 0 to n - 1 do
        (* We should have encountered at least one member of each
           equivalence class up to this point. *)
        assert (t.frequency.(i) >= 1)
      done;
      Array.init (2 * n) ~f:(fun i -> if i < n then t.frequency.(i) else 0)
    in
    t.frequency <- new_frequency );

  t.frequency.(equivalence) <- t.frequency.(equivalence) + 1
;;

let frequency_exn (t : t) (equivalence : Block_equivalence.t) =
  t.frequency.(equivalence |> Block_equivalence.to_int)
;;

let equivalence_exn t (block : Cfg.block) : Block_equivalence.t =
  let symbolic_block =
    Symbolic_block.of_block_exn block t.instruction_index
  in
  Hashtbl.find_exn t.symbolic_block_index symbolic_block
;;

let to_file t ~filename =
  (* We cannot directly marshal Core's hash tables themselves, as they
     contain closures. *)
  let as_alist =
    (* Sanity check, that we did not somehow end up with a discontinuous
       frequency array. *)
    let n_classes = Hashtbl.length t.symbolic_block_index in
    for i = 0 to n_classes - 1 do
      assert (t.frequency.(i) >= 1)
    done;

    for i = n_classes to Array.length t.frequency - 1 do
      assert (t.frequency.(i) = 0)
    done;
    Array.unsafe_truncate t.frequency ~len:n_classes;

    ( Hashtbl.to_alist t.instruction_index.for_basic,
      Hashtbl.to_alist t.instruction_index.for_terminator,
      Hashtbl.to_alist t.symbolic_block_index,
      t.frequency )
  in
  Out_channel.with_file ~binary:true filename ~f:(fun out_channel ->
      Marshal.to_channel out_channel as_alist [])
;;

let of_file ~filename =
  In_channel.with_file ~binary:true filename ~f:(fun inc ->
      let for_basic, for_terminator, symbolic_block_index, frequency =
        Marshal.from_channel inc
      in
      {
        instruction_index =
          {
            Instruction_index.for_basic =
              for_basic
              |> Hashtbl.of_alist_exn (module Instruction_index.For_basic);
            Instruction_index.for_terminator =
              for_terminator
              |> Hashtbl.of_alist_exn
                   (module Instruction_index.For_terminator);
          };
        symbolic_block_index =
          symbolic_block_index
          |> Hashtbl.of_alist_exn (module Symbolic_block);
        frequency;
      })
;;

module Equivalence_metadata = struct
  type t = {
    frequency : int;
    equivalence : Block_equivalence.t;
    representative : Symbolic_block.t;
  }
end

(* XCR gyorsh for ericpts: does Hashtbl.to_alist guarantee that the order of
   pairs in the resulting list is the order of keys? also, wouldn't to_alist
   allocate a huge new list? Another option is to iterate over the hashtable
   and then use random access into the frequency array.

   Array.length symbolic_block breaks the abstraction of how symbolic blocks
   are represented.

   why do you use backticks and not records or named arguments? *)
let filter_and_sort_equivalences t ~min_block_size =
  Hashtbl.filter_mapi t.symbolic_block_index
    ~f:(fun ~key:representative ~data:equivalence ->
      let frequency =
        t.frequency.(equivalence |> Block_equivalence.to_int)
      in
      if Symbolic_block.length representative >= min_block_size then
        Some { Equivalence_metadata.frequency; representative; equivalence }
      else None)
  |> Hashtbl.data
  |> List.sort ~compare:(fun e1 e2 ->
         -Int.compare e1.frequency e2.frequency)
;;

let equivalences_by_frequency t ~min_block_size =
  filter_and_sort_equivalences t ~min_block_size
  |> List.map ~f:(fun e -> e.equivalence)
;;

let print_hashtbl_load_statistics t =
  sprintf "Symbolic_blocks: %d; Instructions: (basic: %d) (terminator: %d)"
    (Hashtbl.length t.symbolic_block_index)
    (Hashtbl.length t.instruction_index.for_basic)
    (Hashtbl.length t.instruction_index.for_terminator)
;;

let print_most_frequent t ~min_block_size ~n_most_frequent_equivalences =
  let filtered =
    List.take
      (filter_and_sort_equivalences t ~min_block_size)
      n_most_frequent_equivalences
  in
  let inverted_basic, inverted_terminator =
    Instruction_index.inverted t.instruction_index
  in
  List.iter filtered ~f:(fun e ->
      printf "Equivalence class %d with frequency %d: \n"
        (e.equivalence |> Block_equivalence.to_int)
        e.frequency;
      Utils.print_block
        (Symbolic_block.invert e.representative ~inverted_basic
           ~inverted_terminator)
        ~block_print_mode:`As_cfg)
;;
