open Core
open Instr_freq

let f ~block =
  Array.find block ~f:(fun instruction ->
      match instruction.Index.With_register_information.desc with
      | Index.Matcher.Basic (Op (Intop_imm (Ior, _))) -> true
      | _ -> false)
;;
