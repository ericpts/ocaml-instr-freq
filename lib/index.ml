open Core
open Ocamlcfg

let hash_fold_array = Utils.hash_fold_array

module type Equivalence = sig
  type t [@@deriving compare, hash, sexp, equal]

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

module Register_equivalence = Int

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
    type t = {
      basics :
        Basic_instruction_equivalence.t With_register_information.t array;
      terminator :
        Terminator_instruction_equivalence.t With_register_information.t;
    }
    [@@deriving compare, sexp_of, hash]

    let of_block_generic
        (b : Cfg.block) ~get_id_for_basic ~get_id_for_terminator =
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
      let basics =
        List.map b.body ~f:(fun basic ->
            let desc = get_id_for_basic basic.desc in
            let arg, res = symbolize_registers_of basic in
            { With_register_information.desc; arg; res })
        |> Array.of_list
      in
      let terminator =
        let desc = get_id_for_terminator b.terminator.desc in
        let arg, res = symbolize_registers_of b.terminator in
        { With_register_information.desc; arg; res }
      in
      { basics; terminator }
    ;;

    let of_block (b : Cfg.block) (instruction_index : Instruction_index.t) =
      of_block_generic b
        ~get_id_for_basic:
          (Instruction_index.get_or_add_id_for_basic instruction_index)
        ~get_id_for_terminator:
          (Instruction_index.get_or_add_id_for_terminator instruction_index)
    ;;

    let of_block_exn
        (b : Cfg.block) (instruction_index : Instruction_index.t) =
      of_block_generic b
        ~get_id_for_basic:(fun basic ->
          Instruction_index.get_id_for_basic instruction_index basic
          |> Option.value_exn)
        ~get_id_for_terminator:(fun terminator ->
          Instruction_index.get_id_for_terminator instruction_index
            terminator
          |> Option.value_exn)
    ;;

    let length (t : t) = 1 + Array.length t.basics
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
  }
end

include T

module Matcher = struct
  type t = {
    basics :
      Basic_instruction_equivalence.t With_register_information.t array
      option;
    terminator :
      Terminator_instruction_equivalence.t With_register_information.t
      option;
  }

  type create_args = {
    with_these_basics :
      Types.From_cfg.basic With_register_information.t list option;
    with_this_terminator :
      Types.From_cfg.terminator With_register_information.t option;
  }
  [@@deriving sexp]

  let create create_args index =
    let basics =
      Option.map create_args.with_these_basics ~f:(fun basics ->
          List.map basics ~f:(fun basic ->
              let desc =
                Instruction_index.get_id_for_basic index.instruction_index
                  basic.desc
                |> Option.value_exn
              in
              { basic with desc })
          |> Array.of_list)
    in
    let terminator =
      Option.map create_args.with_this_terminator ~f:(fun terminator ->
          let desc =
            Instruction_index.get_id_for_terminator index.instruction_index
              terminator.desc
            |> Option.value_exn
          in
          { terminator with desc })
    in
    { basics; terminator }
  ;;

  let matches (t : t) (symbolic_block : Symbolic_block.t) =
    (* We can think of this matching problem as substring search: [ t ]
       contains the pattern, [ symbolic_block ] contains the large string,
       and we want to see if the pattern appears in the large string.

       The instruction id's are the same, however registers are numbered
       from the start of the [ symbolic_block ]: each register is given an
       id, which pretty much corresponds to the first time when we
       encountered it. As such, if an instruction appears in the middle of
       the block, then probably its registers will have high ids.

       Therefore, in order to check if the pattern matches a substring of
       [symbolic_block], we first need to renumber the registers of that
       substring. *)
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
              renumber_registers instruction.With_register_information.arg;
            res = renumber_registers instruction.res;
          })
    in
    let ret = ref true in
    Option.iter t.basics ~f:(fun basics ->
        let pattern = renumber basics in
        let any_match = ref false in
        let k = Array.length pattern in
        for i = 0 to Array.length symbolic_block.basics - k do
          let cur =
            renumber (Array.sub symbolic_block.basics ~pos:i ~len:k)
          in
          (* CR estavarache: Maybe we should also include the terminator's
             registers in this comparison, instead of doing it separately. *)
          let good =
            Array.equal
              (With_register_information.equal
                 Basic_instruction_equivalence.equal)
              pattern cur
          in
          any_match := !any_match || good
        done;
        ret := !ret && !any_match);
    Option.iter t.terminator ~f:(fun terminator ->
        let pattern = renumber [| terminator |] in
        let good = pattern = renumber [| symbolic_block.terminator |] in
        ret := !ret && good);
    !ret
  ;;
end

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

let print_hashtbl_load_statistics t =
  sprintf "Symbolic_blocks: %d; Instructions: (basic: %d) (terminator: %d)"
    (Hashtbl.length t.symbolic_block_index)
    (Hashtbl.length t.instruction_index.for_basic)
    (Hashtbl.length t.instruction_index.for_terminator)
;;
