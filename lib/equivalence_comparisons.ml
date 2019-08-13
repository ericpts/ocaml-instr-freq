open Core
open Ocamlcfg

module Util = struct
  let chain_compare c1 c2 =
    match c1 with
    | 0 -> c2
    | x -> x
  ;;
end

module For_mach = struct
  let compare_test (t1 : Mach.test) (t2 : Mach.test) : int =
    match (t1, t2) with
    | Mach.Iinttest_imm (icomp1, _), Mach.Iinttest_imm (icomp2, _) ->
        Strict_comparisons.For_mach.compare_integer_comparison icomp1 icomp2
    | t1, t2 -> Strict_comparisons.For_mach.compare_test t1 t2
  ;;
end

module For_cfg = struct
  let compare_operation (op1 : Cfg.operation) (op2 : Cfg.operation) : int =
    match (op1, op2) with
    | Const_int _, Const_int _ -> 0
    | Const_float _, Const_float _ -> 0
    | Const_symbol _, Const_symbol _ -> 0
    | Intop_imm (iop1, _), Intop_imm (iop2, _) ->
        Strict_comparisons.For_mach.compare_integer_operation iop1 iop2
    | op1, op2 -> Strict_comparisons.For_cfg.compare_operation op1 op2
  ;;

  let compare_func_call_operation (f1 : Cfg.func_call_operation)
      (f2 : Cfg.func_call_operation) : int =
    match (f1, f2) with
    | Indirect _, Indirect _ -> 0
    | Immediate i1, Immediate i2 -> String.compare i1.func i2.func
    | f1, f2 -> Strict_comparisons.For_cfg.compare_func_call_operation f1 f2
  ;;

  let compare_prim_call_operation (f1 : Cfg.prim_call_operation)
      (f2 : Cfg.prim_call_operation) : int =
    match (f1, f2) with
    | External e1, External e2 ->
        Util.chain_compare
          (String.compare e1.func e2.func)
          (Bool.compare e1.alloc e2.alloc)
    | Alloc a1, Alloc a2 ->
        Util.chain_compare
          (Int.compare a1.bytes a2.bytes)
          (Int.compare a1.spacetime_index a2.spacetime_index)
    | Checkbound c1, Checkbound c2 ->
        Util.chain_compare
          ([%compare: int option] c1.immediate c2.immediate)
          (Int.compare c1.spacetime_index c2.spacetime_index)
    | f1, f2 -> Strict_comparisons.For_cfg.compare_prim_call_operation f1 f2
  ;;

  let compare_basic (basic1 : Cfg.basic) (basic2 : Cfg.basic) : int =
    match (basic1, basic2) with
    | Op op1, Op op2 -> compare_operation op1 op2
    | Call (P p1), Call (P p2) -> compare_prim_call_operation p1 p2
    | Call (F f1), Call (F f2) -> compare_func_call_operation f1 f2
    | basic1, basic2 ->
        Strict_comparisons.For_cfg.compare_basic basic1 basic2
  ;;

  let compare_successor (s1 : Cfg.successor) (s2 : Cfg.successor) : int =
    match (fst s1, fst s2) with
    | Test t1, Test t2 -> For_mach.compare_test t1 t2
    | s1, s2 -> Strict_comparisons.For_cfg.compare_condition s1 s2
  ;;

  let compare_terminator (t1 : Cfg.terminator) (t2 : Cfg.terminator) : int =
    match (t1, t2) with
    | Branch s1, Branch s2 -> List.compare compare_successor s1 s2
    | t1, t2 -> Strict_comparisons.For_cfg.compare_terminator t1 t2
  ;;

  let compare_instruction (i1 : 'a Cfg.instruction)
      (i2 : 'a Cfg.instruction) ~(compare_underlying : 'a -> 'a -> int) :
      int =
    (* CR estavarache: Maybe also check the registers. *)
    compare_underlying i1.desc i2.desc
  ;;

  let compare_basic_instruction =
    compare_instruction ~compare_underlying:compare_basic
  ;;

  let compare_terminator_instruction =
    compare_instruction ~compare_underlying:compare_terminator
  ;;

  let compare_block (block1 : Cfg.block) (block2 : Cfg.block) : int =
    Util.chain_compare
      (List.compare compare_basic_instruction block1.body block2.body)
      (compare_terminator_instruction block1.terminator block2.terminator)
  ;;
end
