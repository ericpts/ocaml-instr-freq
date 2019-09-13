open Instr_freq
open Core

let any_suffix ~(block : Index.Matcher.Whole_block_predicate.block) ~f =
  let rec impl list =
    match list with
    | [] -> false
    | _ :: tail -> if f list then true else impl tail
  in
  impl (Array.to_list block)
;;

let single_use_lookahead
    ~(block : Index.Matcher.Whole_block_predicate.block)
    ~lookahead_length
    ~f =
  let def_use = Def_use.create ~block in
  let lookahead_from_instruction ~instruction_index ~register =
    let rec impl ~index ~register ~length ~acc =
      match
        Def_use.find_uses def_use ~instruction_index:index ~register
      with
      | Uses [| single_use |] -> (
          let next_instruction = block.(single_use) in
          match next_instruction.res with
          | [| single_result |] ->
              let acc = next_instruction :: acc in
              let length = length + 1 in
              if length = lookahead_length then f (List.rev acc)
              else
                impl ~index:single_use ~length ~acc ~register:single_result
          | _ -> false )
      | Uses _ | Register_not_found -> false
    in
    impl ~index:instruction_index ~length:0 ~acc:[] ~register
  in
  try
    Array.iteri block
      ~f:(fun instruction_index { desc = _; arg = _; res } ->
        match res with
        | [| register |] ->
            if lookahead_from_instruction ~instruction_index ~register then
              raise Utils.Stop_iteration
        | _ -> ());
    false
  with Utils.Stop_iteration -> true
;;
