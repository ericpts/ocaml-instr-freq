open Core
open Ocamlcfg

let chain_compare list =
  List.fold_until list ~init:0
    ~f:(fun acc cur ->
      match acc with
      | 0 -> Continue_or_stop.Continue (Lazy.force cur)
      | x -> Continue_or_stop.Stop x)
    ~finish:Fn.id
;;

module Modulo_register_renaming = struct
  exception Registers_differ of int

  let compare_exn (b1 : Cfg.block) (b2 : Cfg.block) : int =
    let get_reg (b : Cfg.block) =
      let terminator_registers =
        Array.append b.terminator.res b.terminator.arg |> Array.to_list
      in
      let body_registers =
        List.map b.body ~f:(fun desc ->
            Array.append desc.arg desc.res |> Array.to_list)
      in
      let all_registers = body_registers @ [ terminator_registers ] in
      List.map all_registers
        ~f:
          (List.map ~f:(fun reg ->
               match reg.Reg.loc with
               | Reg.Unknown -> "unkown"
               | Reg.Reg num -> sprintf "reg#%d" num
               | Stack location -> (
                   match location with
                   | Local x -> sprintf "stack#local#%d" x
                   | Incoming x -> sprintf "stack#incoming#%d" x
                   | Outgoing x -> sprintf "stack#outgoing#%d" x )))
    in
    let renaming_map = Hashtbl.create (module String) in
    try
      List.iter2_exn (get_reg b1) (get_reg b2) ~f:(fun r1 r2 ->
          let len_r1 = List.length r1 in
          let len_r2 = List.length r2 in
          if len_r1 <> len_r2 then
            raise (Registers_differ (Int.compare len_r1 len_r2));

          List.iter2_exn r1 r2 ~f:(fun s1 s2 ->
              Hashtbl.update renaming_map s1 ~f:(function
                | Some s1_in_b2 ->
                    if s1_in_b2 <> s2 then
                      raise (Registers_differ (String.compare s1_in_b2 s2));
                    s2
                | None -> s2)));
      0
    with Registers_differ cmp -> cmp
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
  [@@deriving compare, sexp_of, hash]

  type raise_kind = Cmm.raise_kind =
    | Raise_withtrace
    | Raise_notrace
  [@@deriving compare, sexp_of, hash]
end

module From_arch = struct
  type addressing_mode = Arch.addressing_mode =
    | Ibased of string * int
    | Iindexed of int
    | Iindexed2 of int
    | Iscaled of int * int
    | Iindexed2scaled of int * int
  [@@deriving compare, sexp_of, hash]

  type float_operation = Arch.float_operation =
    | Ifloatadd
    | Ifloatsub
    | Ifloatmul
    | Ifloatdiv
  [@@deriving compare, sexp_of, hash]

  type specific_operation = Arch.specific_operation =
    | Ilea of addressing_mode
    | Istore_int of (nativeint[@compare.ignore]) * addressing_mode * bool
    | Ioffset_loc of int * addressing_mode
    | Ifloatarithmem of float_operation * addressing_mode
    | Ibswap of int
    | Isqrtf
    | Ifloatsqrtf of addressing_mode
    | Isextend32
  [@@deriving compare, sexp_of, hash]
end

module From_lambda = struct
  type integer_comparison = Lambda.integer_comparison =
    | Ceq
    | Cne
    | Clt
    | Cgt
    | Cle
    | Cge
  [@@deriving compare, sexp_of, hash]

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
  [@@deriving compare, sexp_of, hash]
end

module From_mach = struct
  type integer_comparison = Mach.integer_comparison =
    | Isigned of From_lambda.integer_comparison
    | Iunsigned of From_lambda.integer_comparison
  [@@deriving compare, sexp_of, hash]

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
  [@@deriving compare, sexp_of, hash]

  type test = Mach.test =
    | Itruetest
    | Ifalsetest
    | Iinttest of integer_comparison
    | Iinttest_imm of integer_comparison * (int[@compare.ignore])
    | Ifloattest of From_lambda.float_comparison
    | Ioddtest
    | Ieventest
  [@@deriving compare, sexp_of, hash]
end

module From_ident = struct
  type t = Ident.t [@@deriving compare]

  let sexp_of_t t = Ident.name t |> Sexp.of_string

  let hash t = String.hash (Ident.name t)

  let hash_fold_t state t = String.hash_fold_t state (Ident.name t)
end

module From_cfg = struct
  type operation = Cfg.operation =
    | Move
    | Spill
    | Reload
    | Const_int of (nativeint[@compare.ignore])
    | Const_float of (int64[@compare.ignore])
    | Const_symbol of (string[@compare.ignore])
    | Stackoffset of int
    | Load of
        From_cmm.memory_chunk * (From_arch.addressing_mode[@compare.ignore])
    | Store of
        From_cmm.memory_chunk
        * (From_arch.addressing_mode[@compare.ignore])
        * bool
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
      }
  [@@deriving compare, sexp_of, hash]

  type func_call_operation = Cfg.func_call_operation =
    | Indirect of { label_after : int [@compare.ignore] }
    | Immediate of {
        func : string; [@compare.ignore]
        label_after : int; [@compare.ignore]
      }
  [@@deriving compare, sexp_of, hash]

  type prim_call_operation = Cfg.prim_call_operation =
    | External of {
        func : string; [@compare.ignore]
        alloc : bool;
        label_after : int; [@compare.ignore]
      }
    | Alloc of {
        bytes : int;
        label_after_call_gc : int option; [@compare.ignore]
        spacetime_index : int;
      }
    | Checkbound of {
        immediate : int option;
        label_after_error : int option; [@compare.ignore]
        spacetime_index : int;
      }
  [@@deriving compare, sexp_of, hash]

  type call_operation = Cfg.call_operation =
    | P of prim_call_operation
    | F of func_call_operation
  [@@deriving compare, sexp_of, hash]

  type basic = Cfg.basic =
    | Op of operation
    | Call of call_operation
    | Reloadretaddr
    | Entertrap
    | Pushtrap of { lbl_handler : int [@compare.ignore] }
    | Poptrap
    | Prologue
  [@@deriving compare, sexp_of, hash]

  type condition = Cfg.condition =
    | Always
    | Test of From_mach.test
  [@@deriving compare, sexp_of, hash]

  type successor = condition * int [@@deriving compare, sexp_of, hash]

  let hash_fold_array f state array = Array.fold array ~init:state ~f

  type terminator = Cfg.terminator =
    | Branch of (successor list[@compare.ignore])
    | Switch of (int array[@compare.ignore])
    | Return
    | Raise of From_cmm.raise_kind
    | Tailcall of func_call_operation
  [@@deriving compare, sexp_of, hash]

  let compare_instruction (i1 : 'a Cfg.instruction)
      (i2 : 'a Cfg.instruction) ~(compare_underlying : 'a -> 'a -> int) :
      int =
    chain_compare
      [ lazy (compare_underlying i1.desc i2.desc);
        lazy (Int.compare (Array.length i1.arg) (Array.length i2.arg));
        lazy (Int.compare (Array.length i1.res) (Array.length i2.res))
      ]
  ;;

  let compare_basic_instruction =
    compare_instruction ~compare_underlying:compare_basic
  ;;

  let compare_terminator_instruction =
    compare_instruction ~compare_underlying:compare_terminator
  ;;
end
