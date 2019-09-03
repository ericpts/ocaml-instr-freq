open Core
open Ocamlcfg
open Equivalence

let hash_fold_array = Utils.hash_fold_array

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
      type t = Cfg.basic

      let compare = Types.From_cfg.compare_basic

      let sexp_of_t (t : t) = Types.From_cfg.sexp_of_basic t

      let hash (t : t) = Types.From_cfg.hash_basic t
    end

    include T
    include Comparator.Make (T)
  end

  module For_terminator = struct
    module T = struct
      type t = Cfg.terminator

      let compare = Types.From_cfg.compare_terminator

      let sexp_of_t (t : t) = Types.From_cfg.sexp_of_terminator t

      let hash (t : t) = Types.From_cfg.hash_terminator t
    end

    include T
    include Comparator.Make (T)
  end

  type t = {
    for_basic : (Cfg.basic, Basic_instruction_equivalence.t) Hashtbl.t;
    for_terminator :
      (Cfg.terminator, Terminator_instruction_equivalence.t) Hashtbl.t;
  }

  let empty () =
    {
      for_basic = Hashtbl.create (module For_basic);
      for_terminator = Hashtbl.create (module For_terminator);
    }
  ;;

  let get_or_add_id_for_basic (t : t) (instruction : Cfg.basic) =
    get_id_or_add ~equivalence_of_int:Basic_instruction_equivalence.of_int
      t.for_basic instruction
  ;;

  let get_or_add_id_for_terminator (t : t) (instruction : Cfg.terminator) =
    get_id_or_add
      ~equivalence_of_int:Terminator_instruction_equivalence.of_int
      t.for_terminator instruction
  ;;

  let get_id_for_basic (t : t) = Hashtbl.find t.for_basic

  let get_id_for_terminator (t : t) = Hashtbl.find t.for_terminator
end

module With_register_information = struct
  type 'a t = {
    desc : 'a;
    arg : Register_equivalence.t array;
    res : Register_equivalence.t array;
  }
  [@@deriving compare, sexp, hash, equal]
end

(* The symbolic block can be thought of as a representative for an
   equivalence class. I.e., if two block are considered equivalent, then
   they should produce the same symbolic block. *)
module Symbolic_block = struct
  module T = struct
    (* This contains the unique id of every instruction *)
    type t =
      Generic_instruction_equivalence.t With_register_information.t array
    [@@deriving compare, sexp_of, hash]

    let of_block_generic
        (loop_free_block : Loop_free_block.t)
        ~get_id_for_basic
        ~get_id_for_terminator =
      let register_index = Hashtbl.create (module String) in
      let symbolize_registers_of instr =
        let symbolize_register register =
          get_id_or_add ~equivalence_of_int:Register_equivalence.of_int
            register_index
            (Types.Modulo_register_renaming.symbolize_register register
               ~include_register_number:true)
        in
        let arg = Array.map instr.Cfg.arg ~f:symbolize_register in
        let res = Array.map instr.res ~f:symbolize_register in
        (arg, res)
      in
      Array.concat_map
        (Loop_free_block.to_list loop_free_block |> Array.of_list_rev)
        ~f:(fun block ->
          List.map block.body ~f:(fun basic ->
              let desc =
                get_id_for_basic basic.desc
                |> Generic_instruction_equivalence.of_basic
              in
              let arg, res = symbolize_registers_of basic in
              { With_register_information.desc; arg; res })
          @ [ (let desc =
                 get_id_for_terminator block.terminator.desc
                 |> Generic_instruction_equivalence.of_terminator
               in
               let arg, res = symbolize_registers_of block.terminator in
               { With_register_information.desc; arg; res })
            ]
          |> Array.of_list)
    ;;

    let of_block
        (loop_free_block : Loop_free_block.t)
        (instruction_index : Instruction_index.t) =
      of_block_generic loop_free_block
        ~get_id_for_basic:
          (Instruction_index.get_or_add_id_for_basic instruction_index)
        ~get_id_for_terminator:
          (Instruction_index.get_or_add_id_for_terminator instruction_index)
    ;;

    let of_block_exn
        (loop_free_block : Loop_free_block.t)
        (instruction_index : Instruction_index.t) =
      of_block_generic loop_free_block
        ~get_id_for_basic:(fun basic ->
          Instruction_index.get_id_for_basic instruction_index basic
          |> Option.value_exn)
        ~get_id_for_terminator:(fun terminator ->
          Instruction_index.get_id_for_terminator instruction_index
            terminator
          |> Option.value_exn)
    ;;

    let length (t : t) = Array.length t
  end

  include T
  include Comparator.Make (T)
end

module T = struct
  type t = {
    instruction_index : Instruction_index.t;
    symbolic_block_index :
      (Symbolic_block.t, Block_equivalence.t) Hashtbl.t;
    mutable frequency : int Array.t;
    mutable sample_file : Filename.t Array.t;
  }
end

include T

module Matcher = struct
  type t =
    Generic_instruction_equivalence.t With_register_information.t Array.t

  type desc =
    | Basic of Types.From_cfg.basic
    | Terminator of Types.From_cfg.terminator
  [@@deriving sexp]

  let renumber instructions =
    let register_index = Hashtbl.create (module Int) in
    let renumber_registers registers =
      Array.map registers
        ~f:
          (get_id_or_add register_index
             ~equivalence_of_int:Register_equivalence.of_int)
    in
    Array.map instructions ~f:(fun instruction ->
        {
          instruction with
          With_register_information.arg =
            renumber_registers
              (Array.map instruction.With_register_information.arg
                 ~f:Register_equivalence.to_int);
          res =
            renumber_registers
              (Array.map instruction.res ~f:Register_equivalence.to_int);
        })
  ;;

  let create (instructions : desc With_register_information.t list) index =
    Array.map (Array.of_list instructions) ~f:(fun instruction ->
        let desc =
          match instruction.desc with
          | Basic basic ->
              Instruction_index.get_id_for_basic index.instruction_index
                basic
              |> Option.value_exn
                   ~error:
                     (Error.of_thunk (fun () ->
                          sprintf
                            !"Matcher contains basic instruction not \
                              present in the index:\n\
                              %{sexp: Types.From_cfg.basic}!"
                            basic))
              |> Generic_instruction_equivalence.of_basic
          | Terminator terminator ->
              Instruction_index.get_id_for_terminator
                index.instruction_index terminator
              |> Option.value_exn
                   ~error:
                     (Error.of_thunk (fun () ->
                          sprintf
                            !"Matcher contains terminator not present in \
                              the index:\n\
                              %{sexp: Types.From_cfg.terminator}"
                            terminator))
              |> Generic_instruction_equivalence.of_terminator
        in
        { instruction with desc })
    |> renumber
  ;;

  (** We can think of this matching problem as substring search: [t]
      contains the pattern, [symbolic_block] contains the large string, and
      we want to see if the pattern appears in the large string.

      The instruction id's are the same, however registers are numbered from
      the start of the [symbolic_block]: each register is given an id, which
      pretty much corresponds to the first time when we encountered it. As
      such, if an instruction appears in the middle of the block, then
      probably its registers will have high ids.

      Therefore, in order to check if the pattern matches a substring of
      [symbolic_block], we first need to renumber the registers of that
      substring. *)
  let matches (t : t) (symbolic_block : Symbolic_block.t) =
    let k = Array.length t in
    try
      for i = 0 to Array.length symbolic_block - k do
        let cur = renumber (Array.sub symbolic_block ~pos:i ~len:k) in
        let good =
          Array.equal
            (With_register_information.equal
               Generic_instruction_equivalence.equal)
            t cur
        in
        if good then raise Utils.Stop_iteration
      done;
      false
    with Utils.Stop_iteration -> true
  ;;
end

let empty () =
  {
    instruction_index = Instruction_index.empty ();
    symbolic_block_index = Hashtbl.create (module Symbolic_block);
    frequency = Array.create ~len:200_000 0;
    sample_file = Array.create ~len:200_000 "";
  }
;;

let double_array arr ~default =
  let n = Array.length arr in
  Array.init (2 * n) ~f:(fun i -> if i < n then arr.(i) else default)
;;

let update ?source_file t (loop_free_block : Loop_free_block.t) =
  let symbolic_block =
    Symbolic_block.of_block loop_free_block t.instruction_index
  in
  let equivalence =
    get_id_or_add t.symbolic_block_index symbolic_block
      ~equivalence_of_int:Block_equivalence.of_int
    |> Block_equivalence.to_int
  in
  if equivalence >= Array.length t.frequency then (
    assert (
      Hashtbl.length t.symbolic_block_index = Array.length t.frequency + 1
    );
    t.frequency <- double_array t.frequency ~default:0;
    t.sample_file <- double_array t.sample_file ~default:"" );

  t.frequency.(equivalence) <- t.frequency.(equivalence) + 1;
  Option.iter source_file ~f:(fun file ->
      t.sample_file.(equivalence) <- file)
;;

let frequency_exn (t : t) (equivalence : Block_equivalence.t) =
  t.frequency.(equivalence |> Block_equivalence.to_int)
;;

let equivalence_exn t (loop_free_block : Loop_free_block.t) :
    Block_equivalence.t =
  let symbolic_block =
    Symbolic_block.of_block_exn loop_free_block t.instruction_index
  in
  Hashtbl.find_exn t.symbolic_block_index symbolic_block
;;

let to_file t ~filename =
  (* Saving the index to disk implies incurring a 2x memory load, and in
     some cases this would outright kill the program. To try our best to fit
     in memory, do a full compaction before saving. *)
  Gc.compact ();

  (* We cannot directly marshal Core's hash tables themselves, as they
     contain closures. *)
  let as_alist =
    (* Sanity check, that we did not somehow end up with a discontinuous
       frequency array. *)
    let n_classes = Hashtbl.length t.symbolic_block_index in
    if n_classes = 0 then
      failwith
        "Index contains zero equivalence classes! Either the files are \
         empty, or the context length is too large.";

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
      t.frequency,
      t.sample_file )
  in
  Out_channel.with_file ~binary:true filename ~f:(fun out_channel ->
      Marshal.to_channel out_channel as_alist [])
;;

let of_file ~filename =
  In_channel.with_file ~binary:true filename ~f:(fun inc ->
      let ( for_basic,
            for_terminator,
            symbolic_block_index,
            frequency,
            sample_file ) =
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
        sample_file;
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
let filter_and_sort_equivalences t ~min_block_size ~matcher =
  Hashtbl.filter_mapi t.symbolic_block_index
    ~f:(fun ~key:representative ~data:equivalence ->
      let frequency =
        t.frequency.(equivalence |> Block_equivalence.to_int)
      in
      if
        Symbolic_block.length representative >= min_block_size
        && Option.value_map matcher ~default:true ~f:(fun matcher ->
               Matcher.matches matcher representative)
      then
        Some { Equivalence_metadata.frequency; representative; equivalence }
      else None)
  |> Hashtbl.data
  |> List.sort ~compare:(fun e1 e2 ->
         -Int.compare e1.frequency e2.frequency)
;;

let equivalences_by_frequency t ~min_block_size ~matcher =
  filter_and_sort_equivalences t ~min_block_size ~matcher
  |> List.map ~f:(fun e -> e.equivalence)
;;

type hashtbl_load_statistics = {
  n_symbolic_blocks : int;
  n_basic_instructions : int;
  n_terminator_instructions : int;
}

let hashtbl_load_statistics t =
  {
    n_symbolic_blocks = Hashtbl.length t.symbolic_block_index;
    n_basic_instructions = Hashtbl.length t.instruction_index.for_basic;
    n_terminator_instructions =
      Hashtbl.length t.instruction_index.for_terminator;
  }
;;

let sample_file t equivalence =
  match
    t.sample_file.(Equivalence.Block_equivalence.to_int equivalence)
  with
  | "" -> None
  | x -> Some x
;;
