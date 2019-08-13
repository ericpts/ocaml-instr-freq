open Core
open! Ocamlcfg

module Default_comparisons = struct
  module For_cmm = struct
    type memory_chunk = Cmm.memory_chunk =
      | Byte_unsigned
      | Byte_signed
      | Sixteen_unsigned
      | Sixteen_signed
      | Thirtytwo_unsigned
      | Thirtytwo_signed
      | Word_int
      | Word_val
      | Single
      | Double
      | Double_u
    [@@deriving compare, sexp_of]

    type raise_kind = Cmm.raise_kind =
      | Raise_withtrace
      | Raise_notrace
    [@@deriving compare, sexp_of]
  end

  module For_arch = struct
    type addressing_mode = Arch.addressing_mode =
      | Ibased of string * int
      | Iindexed of int
      | Iindexed2 of int
      | Iscaled of int * int
      | Iindexed2scaled of int * int
    [@@deriving compare, sexp_of]

    type float_operation = Arch.float_operation =
      | Ifloatadd
      | Ifloatsub
      | Ifloatmul
      | Ifloatdiv
    [@@deriving compare, sexp_of]

    type specific_operation = Arch.specific_operation =
      | Ilea of addressing_mode
      | Istore_int of nativeint * addressing_mode * bool
      | Ioffset_loc of int * addressing_mode
      | Ifloatarithmem of float_operation * addressing_mode
      | Ibswap of int
      | Isqrtf
      | Ifloatsqrtf of addressing_mode
      | Isextend32
    [@@deriving compare, sexp_of]
  end

  module For_lambda = struct
    type integer_comparison = Lambda.integer_comparison =
      | Ceq
      | Cne
      | Clt
      | Cgt
      | Cle
      | Cge
    [@@deriving compare, sexp_of]

    type float_comparison = Lambda.float_comparison =
      | CFeq
      | CFneq
      | CFlt
      | CFnlt
      | CFgt
      | CFngt
      | CFle
      | CFnle
      | CFge
      | CFnge
    [@@deriving compare, sexp_of]
  end

  module For_mach = struct
    type integer_comparison = Mach.integer_comparison =
      | Isigned of For_lambda.integer_comparison
      | Iunsigned of For_lambda.integer_comparison
    [@@deriving compare, sexp_of]

    type integer_operation = Mach.integer_operation =
      | Iadd
      | Isub
      | Imul
      | Imulh
      | Idiv
      | Imod
      | Iand
      | Ior
      | Ixor
      | Ilsl
      | Ilsr
      | Iasr
      | Icomp of integer_comparison
      | Icheckbound of {
          label_after_error : int sexp_option;
          spacetime_index : int;
        }
    [@@deriving compare, sexp_of]

    type test = Mach.test =
      | Itruetest
      | Ifalsetest
      | Iinttest of integer_comparison
      | Iinttest_imm of integer_comparison * int
      | Ifloattest of For_lambda.float_comparison
      | Ioddtest
      | Ieventest
    [@@deriving compare, sexp_of]
  end

  module For_ident = struct
    type t = Ident.t [@@deriving compare]

    let sexp_of_t t = Ident.name t |> Sexp.of_string
  end

  module For_cfg = struct
    type operation = Cfg.operation =
      | Move
      | Spill
      | Reload
      | Const_int of nativeint
      | Const_float of int64
      | Const_symbol of string
      | Stackoffset of int
      | Load of For_cmm.memory_chunk * For_arch.addressing_mode
      | Store of For_cmm.memory_chunk * For_arch.addressing_mode * bool
      | Intop of For_mach.integer_operation
      | Intop_imm of For_mach.integer_operation * int
      | Negf
      | Absf
      | Addf
      | Subf
      | Mulf
      | Divf
      | Floatofint
      | Intoffloat
      | Specific of For_arch.specific_operation
      | Name_for_debugger of {
          ident : For_ident.t;
          which_parameter : int option;
          provenance : unit option;
          is_assignment : bool;
        }
    [@@deriving compare, sexp_of]

    type func_call_operation = Cfg.func_call_operation =
      | Indirect of { label_after : int }
      | Immediate of {
          func : string;
          label_after : int;
        }
    [@@deriving compare, sexp_of]

    type prim_call_operation = Cfg.prim_call_operation =
      | External of {
          func : string;
          alloc : bool;
          label_after : int;
        }
      | Alloc of {
          bytes : int;
          label_after_call_gc : int option;
          spacetime_index : int;
        }
      | Checkbound of {
          immediate : int option;
          label_after_error : int option;
          spacetime_index : int;
        }
    [@@deriving compare, sexp_of]

    type call_operation = Cfg.call_operation =
      | P of prim_call_operation
      | F of func_call_operation
    [@@deriving compare, sexp_of]

    type basic = Cfg.basic =
      | Op of operation
      | Call of call_operation
      | Reloadretaddr
      | Entertrap
      | Pushtrap of { lbl_handler : int }
      | Poptrap
      | Prologue
    [@@deriving compare, sexp_of]

    type condition = Cfg.condition =
      | Always
      | Test of For_mach.test
    [@@deriving compare, sexp_of]

    type successor = condition * int [@@deriving compare, sexp_of]

    type terminator = Cfg.terminator =
      | Branch of successor list
      | Switch of int array
      | Return
      | Raise of For_cmm.raise_kind
      | Tailcall of func_call_operation
    [@@deriving compare, sexp_of]
  end
end

module Equivalence_comparisons = struct
  module For_mach = struct
    let compare_test (t1 : Mach.test) (t2 : Mach.test) : int =
      match (t1, t2) with
      | Mach.Iinttest_imm (icomp1, _), Mach.Iinttest_imm (icomp2, _) ->
          Default_comparisons.For_mach.compare_integer_comparison icomp1
            icomp2
      | t1, t2 -> Default_comparisons.For_mach.compare_test t1 t2
    ;;
  end

  module For_cfg = struct
    let compare_operation (op1 : Cfg.operation) (op2 : Cfg.operation) : int
        =
      match (op1, op2) with
      | Const_int _, Const_int _ -> 0
      | Const_float _, Const_float _ -> 0
      | Const_symbol _, Const_symbol _ -> 0
      | Intop_imm (iop1, _), Intop_imm (iop2, _) ->
          Default_comparisons.For_mach.compare_integer_operation iop1 iop2
      | op1, op2 -> Default_comparisons.For_cfg.compare_operation op1 op2
    ;;

    let compare_basic (basic1 : Cfg.basic) (basic2 : Cfg.basic) : int =
      match (basic1, basic2) with
      | Op op1, Op op2 -> compare_operation op1 op2
      | basic1, basic2 ->
          Default_comparisons.For_cfg.compare_basic basic1 basic2
    ;;

    let compare_successor (s1 : Cfg.successor) (s2 : Cfg.successor) : int =
      match (fst s1, fst s2) with
      | Test t1, Test t2 -> For_mach.compare_test t1 t2
      | s1, s2 -> Default_comparisons.For_cfg.compare_condition s1 s2
    ;;

    let compare_terminator (t1 : Cfg.terminator) (t2 : Cfg.terminator) : int
        =
      match (t1, t2) with
      | Branch s1, Branch s2 -> List.compare compare_successor s1 s2
      | t1, t2 -> Default_comparisons.For_cfg.compare_terminator t1 t2
    ;;

    let compare_instruction (i1 : 'a Cfg.instruction)
        (i2 : 'a Cfg.instruction) ~(compare_underlying : 'a -> 'a -> int) :
        int =
      match compare_underlying i1.desc i2.desc with
      | 0 ->
          (* CR estavarache: Figure out how to check the registers too*)
          0
      | nonequal -> nonequal
    ;;

    let compare_basic_instruction =
      compare_instruction ~compare_underlying:compare_basic
    ;;

    let compare_terminator_instruction =
      compare_instruction ~compare_underlying:compare_terminator
    ;;
  end
end
