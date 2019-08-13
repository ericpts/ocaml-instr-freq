open Core
open Ocamlcfg

module Default_comparisons : sig
  module For_cmm : sig
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
  end

  module For_arch : sig
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

  module For_lambda : sig
    type integer_comparison = Lambda.integer_comparison =
      | Ceq
      | Cne
      | Clt
      | Cgt
      | Cle
      | Cge
    [@@deriving compare, sexp_of]
  end

  module For_mach : sig
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
  end

  module For_ident : sig
    type t = Ident.t [@@deriving compare]

    val sexp_of_t : t -> Sexp.t
  end

  module For_cfg : sig
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
  end
end

module Equivalence_comparisons : sig
  module For_cfg : sig
    val compare_operation : Cfg.operation -> Cfg.operation -> int

    val compare_basic : Cfg.basic -> Cfg.basic -> int

    val compare_basic_instruction :
      Cfg.basic Cfg.instruction -> Cfg.basic Cfg.instruction -> int
  end
end
