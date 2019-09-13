open Instr_freq
open Core
open Equivalence

let any_suffix ~(block : Index.Matcher.Whole_block_predicate.block) ~f =
  let rec impl list =
    match list with
    | [] -> false
    | _ :: tail -> if f list then true else impl tail
  in
  impl (Array.to_list block)
;;

module Def_use : sig
  type t

  type uses =
    | Uses of int Array.t
    | Register_not_found
  [@@deriving sexp]

  type def =
    | At_instruction of int
    | Not_defined_in_this_block
    | Register_not_found
  [@@deriving sexp]

  val create : block:Index.Matcher.Whole_block_predicate.block -> t

  val find_uses :
    t -> instruction_index:int -> register:Register_equivalence.t -> uses

  val find_def :
    t -> instruction_index:int -> register:Register_equivalence.t -> def
end = struct
  type t = {
    block : Index.Matcher.Whole_block_predicate.block;
    var_for_register :
      (* For each instruction of the block, store for each register its var
         id. *)
      (Register_equivalence.t, int) Hashtbl.t Array.t;
    def_for_var : (int, int) Hashtbl.t;
    uses_for_var : (int, int Array.t) Hashtbl.t;
  }

  type uses =
    | Uses of int Array.t
    | Register_not_found
  [@@deriving sexp]

  type def =
    | At_instruction of int
    | Not_defined_in_this_block
    | Register_not_found
  [@@deriving sexp]

  let create ~(block : Index.Matcher.Whole_block_predicate.block) =
    let var_for_register =
      Array.init (Array.length block) ~f:(fun _ ->
          Hashtbl.create (module Register_equivalence))
    in
    let def_for_var = Hashtbl.create (module Int) in
    let uses_for_var = Hashtbl.create (module Int) in
    let next_available_var =
      let var = ref 0 in
      let f () =
        let ret = !var in
        var := ret + 1;
        ret
      in
      f
    in
    let on_use index reg =
      let var =
        match Hashtbl.find var_for_register.(index) reg with
        | Some var -> var
        | None ->
            let var = next_available_var () in
            Hashtbl.set var_for_register.(index) ~key:reg ~data:var;
            var
      in
      Hashtbl.update uses_for_var var ~f:(fun maybe_list ->
          index :: Option.value maybe_list ~default:[])
    in
    let on_def index reg =
      let var = next_available_var () in
      Hashtbl.set var_for_register.(index) ~key:reg ~data:var;
      Hashtbl.set def_for_var ~key:var ~data:index
    in
    Array.iteri block ~f:(fun index { desc = _; arg; res } ->
        if index >= 1 then
          var_for_register.(index) <-
            Hashtbl.copy var_for_register.(index - 1);

        Array.iter arg ~f:(on_use index);
        Array.iter res ~f:(on_def index));

    let uses_for_var =
      Hashtbl.map uses_for_var ~f:(fun descending_occurance_list ->
          Array.of_list_rev descending_occurance_list)
    in
    { block; var_for_register; def_for_var; uses_for_var }
  ;;

  let find_def
      (t : t)
      ~(instruction_index : int)
      ~(register : Register_equivalence.t) =
    match Hashtbl.find t.var_for_register.(instruction_index) register with
    | Some var -> (
        match Hashtbl.find t.def_for_var var with
        | Some index -> At_instruction index
        | None -> Not_defined_in_this_block )
    | None -> Register_not_found
  ;;

  let find_uses
      (t : t)
      ~(instruction_index : int)
      ~(register : Register_equivalence.t) =
    match Hashtbl.find t.var_for_register.(instruction_index) register with
    | Some var ->
        Uses (Hashtbl.find t.uses_for_var var |> Option.value ~default:[||])
    | None -> Register_not_found
  ;;

  module Tests = struct
    open Types.From_cfg
    open Types.From_mach
    open Index.Matcher

    let to_register_array array =
      Array.of_list_map array ~f:Register_equivalence.of_int
    ;;

    let make_int_op op ~arg ~res =
      {
        Index.With_register_information.desc = Basic (Op (Intop op));
        arg = [ arg ] |> to_register_array;
        res = [ res ] |> to_register_array;
      }
    ;;

    let make_move ~arg ~res =
      {
        Index.With_register_information.desc = Basic (Op Move);
        arg = [ arg ] |> to_register_array;
        res = [ res ] |> to_register_array;
      }
    ;;

    let%expect_test "simple" =
      let block =
        [| make_int_op Iadd ~arg:0 ~res:0; make_int_op Isub ~arg:0 ~res:1 |]
      in
      let def_use = create ~block in
      printf !"Def: %{sexp: def}\n"
        (find_def def_use ~instruction_index:0
           ~register:(Register_equivalence.of_int 0));
      printf !"Uses: %{sexp: uses}\n"
        (find_uses def_use ~instruction_index:0
           ~register:(Register_equivalence.of_int 0));
      [%expect {|
      Def: (At_instruction 0)
      Uses: (Uses (1))
|}]
    ;;

    let%expect_test "long" =
      let block =
        [| make_int_op Iadd ~arg:0 ~res:0;
           make_int_op Iadd ~arg:0 ~res:1;
           make_int_op Iadd ~arg:0 ~res:2;
           make_int_op Iadd ~arg:0 ~res:3;
           make_move ~arg:1 ~res:0;
           make_int_op Iadd ~arg:0 ~res:1;
           make_int_op Iadd ~arg:0 ~res:2;
           make_int_op Iadd ~arg:0 ~res:3
        |]
      in
      let def_use = create ~block in
      printf
        !"First Def: %{sexp: def}\n"
        (find_def def_use ~instruction_index:0
           ~register:(Register_equivalence.of_int 0));
      printf
        !"First Uses: %{sexp: uses}\n"
        (find_uses def_use ~instruction_index:0
           ~register:(Register_equivalence.of_int 0));
      printf
        !"Second Def: %{sexp: def}\n"
        (find_def def_use ~instruction_index:4
           ~register:(Register_equivalence.of_int 0));
      printf
        !"Second Uses: %{sexp: uses}\n"
        (find_uses def_use ~instruction_index:4
           ~register:(Register_equivalence.of_int 0));
      [%expect
        {|
      First Def: (At_instruction 0)
      First Uses: (Uses (1 2 3))
      Second Def: (At_instruction 4)
      Second Uses: (Uses (5 6 7))
|}]
    ;;
  end
end
