(* This file contains redefinitions of the compiler types, plus useful
   @@deriving statements, which make them easier to use overall, and also
   makes integration with Core easier. *)
open Core
module Cfg = Ocamlcfg.Cfg

let chain_compare list =
  List.fold_until list ~init:0
    ~f:(fun acc cur ->
      match acc with
      | 0 -> Continue_or_stop.Continue (Lazy.force cur)
      | x -> Continue_or_stop.Stop x)
    ~finish:Fn.id
;;

module Modulo_register_renaming = struct
  let symbolize_register (reg : Reg.t) ~include_register_number =
    let maybe_number string num =
      match include_register_number with
      | true -> sprintf "%s#%d" string num
      | false -> string
    in
    match reg.Reg.loc with
    | Reg.Unknown -> "unknown"
    | Reg.Reg num -> maybe_number "reg" num
    | Stack location -> (
        match location with
        | Local num -> maybe_number "stack#local" num
        | Incoming num -> maybe_number "stack#incoming" num
        | Outgoing num -> maybe_number "stack#outgoing" num )
  ;;
end

module From_cmm = struct
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
  [@@deriving compare, sexp, hash]

  type raise_kind = Cmm.raise_kind =
    | Raise_withtrace
    | Raise_notrace
  [@@deriving compare, sexp, hash]
end

(* XCR gyorsh for ericpts: Arch.addressing_mode & co won't build when the
   compile is configured for other targets. It's okay for our purposes, but
   you need to state the limitation somewhere earlier.

   ericpts: Solved in the readme. *)
module From_arch = struct
  type addressing_mode = Arch.addressing_mode =
    | Ibased of (string[@compare.ignore]) * (int[@compare.ignore])
    | Iindexed of (int[@compare.ignore])
    | Iindexed2 of (int[@compare.ignore])
    | Iscaled of (int[@compare.ignore]) * (int[@compare.ignore])
    | Iindexed2scaled of (int[@compare.ignore]) * (int[@compare.ignore])
  [@@deriving compare, sexp, hash]

  type float_operation = Arch.float_operation =
    | Ifloatadd
    | Ifloatsub
    | Ifloatmul
    | Ifloatdiv
  [@@deriving compare, sexp, hash]

  type specific_operation = Arch.specific_operation =
    | Ilea of addressing_mode
    | Istore_int of (nativeint[@compare.ignore]) * addressing_mode * bool
    | Ioffset_loc of int * addressing_mode
    | Ifloatarithmem of float_operation * addressing_mode
    | Ibswap of int
    | Isqrtf
    | Ifloatsqrtf of addressing_mode
    | Isextend32
  [@@deriving compare, sexp, hash]
end

module From_lambda = struct
  type integer_comparison = Lambda.integer_comparison =
    | Ceq
    | Cne
    | Clt
    | Cgt
    | Cle
    | Cge
  [@@deriving compare, sexp, hash]

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
  [@@deriving compare, sexp, hash]
end

module From_mach = struct
  type integer_comparison = Mach.integer_comparison =
    | Isigned of From_lambda.integer_comparison
    | Iunsigned of From_lambda.integer_comparison
  [@@deriving compare, sexp, hash]

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
        label_after_error : int option; [@sexp.option] [@compare.ignore]
        spacetime_index : int; [@compare.ignore]
      }
  [@@deriving compare, sexp, hash]

  type test = Mach.test =
    | Itruetest
    | Ifalsetest
    | Iinttest of integer_comparison
    | Iinttest_imm of integer_comparison * (int[@compare.ignore])
    | Ifloattest of From_lambda.float_comparison
    | Ioddtest
    | Ieventest
  [@@deriving compare, sexp, hash]
end

module From_ident = struct
  type t = Ident.t [@@deriving compare]

  let sexp_of_t t = Ident.name t |> Sexp.of_string

  let t_of_sexp sexp =
    (* CR estavarache: This is a hack, in that create_local s will return
       different results if called twice with the same argument. *)
    String.t_of_sexp sexp |> Ident.create_local
  ;;

  let hash t = String.hash (Ident.name t)

  let hash_fold_t state t = String.hash_fold_t state (Ident.name t)
end

(* XCR gyorsh for ericpts: @compare.ignore

   addressing_mode should not be ignored. Compare because different variants
   result in different assembly instructions (or operand format), but the
   specific int offsets inside it can be ignored for now.

   The contents of Name_for_debugger need to be ignored.

   spacetime_index and Checkbound.immediate can be ignored.

   The second component of successor represents the label of the successor
   block, so it should also be ignored, but not the condition. Length of the
   successor array shouldn't be ignored. Similarly, switch terminator
   ideally should compare the length of the array. *)
module From_cfg = struct
  type operation = Cfg.operation =
    | Move
    | Spill
    | Reload
    | Const_int of (nativeint[@compare.ignore])
    | Const_float of (int64[@compare.ignore])
    | Const_symbol of (string[@compare.ignore])
    | Stackoffset of int
    | Load of From_cmm.memory_chunk * From_arch.addressing_mode
    | Store of From_cmm.memory_chunk * From_arch.addressing_mode * bool
    | Intop of From_mach.integer_operation
    | Intop_imm of From_mach.integer_operation * (int[@compare.ignore])
    | Negf
    | Absf
    | Addf
    | Subf
    | Mulf
    | Divf
    | Floatofint
    | Intoffloat
    | Specific of From_arch.specific_operation
    | Name_for_debugger of {
        ident : From_ident.t;
        which_parameter : int option;
        provenance : unit option;
        is_assignment : bool;
      } [@compare.ignore]
  [@@deriving compare, sexp, hash]

  type func_call_operation = Cfg.func_call_operation =
    | Indirect of { label_after : int [@compare.ignore] }
    | Direct of {
        func_symbol : string; [@compare.ignore]
        label_after : int; [@compare.ignore]
      }
  [@@deriving compare, sexp, hash]

  type tail_call_operation = Cfg.tail_call_operation =
    | Self of { label_after : int [@compare.ignore] }
    | Func of func_call_operation
  [@@deriving compare, sexp, hash]

  type prim_call_operation = Cfg.prim_call_operation =
    | External of {
        func_symbol : string; [@compare.ignore]
        alloc : bool;
        label_after : int; [@compare.ignore]
      }
    | Alloc of {
        bytes : int;
        label_after_call_gc : int option; [@compare.ignore]
        spacetime_index : int; [@compare.ignore]
      }
    | Checkbound of {
        immediate : int option; [@compare.ignore]
        label_after_error : int option; [@compare.ignore]
        spacetime_index : int; [@compare.ignore]
      }
  [@@deriving compare, sexp, hash]

  type call_operation = Cfg.call_operation =
    | P of prim_call_operation
    | F of func_call_operation
  [@@deriving compare, sexp, hash]

  type basic = Cfg.basic =
    | Op of operation
    | Call of call_operation
    | Reloadretaddr
    | Pushtrap of { lbl_handler : int [@compare.ignore] }
    | Poptrap
    | Prologue
  [@@deriving compare, sexp, hash]

  type condition = Cfg.condition =
    | Always
    | Test of From_mach.test
  [@@deriving compare, sexp, hash]

  type successor = condition * (int[@compare.ignore])
  [@@deriving compare, sexp, hash]

  let hash_fold_array = Utils.hash_fold_array

  type switch_array = int array [@@deriving sexp]

  let compare_switch_array arr1 arr2 =
    Int.compare (Array.length arr1) (Array.length arr2)
  ;;

  let hash_fold_switch_array state array =
    Int.hash_fold_t state (Array.length array)
  ;;

  type terminator = Cfg.terminator =
    | Branch of successor list
    | Switch of switch_array
    | Return
    | Raise of From_cmm.raise_kind
    | Tailcall of tail_call_operation
  [@@deriving compare, sexp, hash]

  (* XCR gyorsh for ericpts: I think I confused you about it earlier. We do
     need to check that i1.arg and i1.res use the same variant of Reg.loc as
     the corresponding entries in i2. Location can be either Register or
     Stack, in which case a memory indexing operand is emitted that refers
     to a precomputed stack offset. We don't care what the offset is, but we
     do care whether the operand is in a register or on the stack, which is
     often a different instruction encoding so we may. I don't think we need
     to check Proc.register_class for amd64 backend, because there are only
     2 (float and everything else), and float is only used in special float
     instructions. *)
  let compare_instruction
      (i1 : 'a Cfg.instruction)
      (i2 : 'a Cfg.instruction)
      ~(compare_underlying : 'a -> 'a -> int) : int =
    let compare_reg_arrays arr1 arr2 =
      let symbolize =
        Modulo_register_renaming.symbolize_register
          ~include_register_number:false
      in
      Array.compare String.compare
        (Array.map arr1 ~f:symbolize)
        (Array.map arr2 ~f:symbolize)
    in
    chain_compare
      [ lazy (compare_underlying i1.desc i2.desc);
        lazy (compare_reg_arrays i1.arg i2.arg);
        lazy (compare_reg_arrays i1.res i2.res)
      ]
  ;;

  let compare_basic_instruction =
    compare_instruction ~compare_underlying:compare_basic
  ;;

  let compare_terminator_instruction =
    compare_instruction ~compare_underlying:compare_terminator
  ;;
end
