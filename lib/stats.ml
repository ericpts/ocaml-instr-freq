open Core
open Equivalence

type t = {
  on_block :
    Loop_free_block.t ->
    file:string ->
    equivalence:Block_equivalence.t ->
    frequency:int ->
    fundecl:Linear.fundecl ->
    [ `Stop | `Continue ];
  on_finish_iteration : unit -> unit;
}

let combine ts =
  let active_ts = ref ts in
  let on_block block ~file ~equivalence ~frequency ~fundecl =
    active_ts :=
      List.filter !active_ts ~f:(fun t ->
          match t.on_block block ~file ~equivalence ~frequency ~fundecl with
          | `Continue -> true
          | `Stop -> false);
    if List.length !active_ts > 0 then `Continue else `Stop
  in
  let on_finish_iteration () =
    List.iter ts ~f:(fun t -> t.on_finish_iteration ())
  in
  { on_block; on_finish_iteration }
;;

let print_most_popular_classes
    index
    ~n_most_frequent_equivalences
    ~n_real_blocks_to_print
    ~block_print_mode
    ~min_block_size
    ~matcher =
  let equivalences_to_print =
    List.take
      (Index.equivalences_by_frequency index ~min_block_size ~matcher)
      n_most_frequent_equivalences
  in
  let remaining_to_print =
    if n_real_blocks_to_print = 0 then
      Hashtbl.create (module Block_equivalence)
    else
      match
        Hashtbl.create_mapped
          (module Block_equivalence)
          ~get_key:Fn.id
          ~get_data:(Fn.const n_real_blocks_to_print)
          equivalences_to_print
      with
      | `Ok hashtbl -> hashtbl
      | `Duplicate_keys list ->
          failwithf
            !"Received unexpected duplicated equivalence indices: %{sexp: \
              Block_equivalence.t list} "
            list ()
  in
  let on_block
      block ~file ~equivalence ~frequency ~(fundecl : Linear.fundecl) =
    match Hashtbl.find remaining_to_print equivalence with
    | None -> `Continue
    | Some n ->
        printf
          "Equivalence %s with %d members;\n\tfile: %s\n\tfunction: %s\n"
          ( sprintf "<%d>" (Block_equivalence.to_int equivalence)
          |> Utils.color Cyan )
          frequency
          (Utils.color Utils.Cyan file)
          (Utils.color Utils.Cyan fundecl.fun_name);
        Loop_free_block.print block block_print_mode;
        printf "%!";
        let rem = n - 1 in
        if rem > 0 then
          Hashtbl.set remaining_to_print ~key:equivalence ~data:rem
        else Hashtbl.remove remaining_to_print equivalence;
        if Hashtbl.length remaining_to_print > 0 then `Continue else `Stop
  in
  let on_finish_iteration () = () in
  { on_block; on_finish_iteration }
;;

let count_blocks_matching index ~min_block_size ~matcher =
  let matching_equivalences =
    Index.equivalences_by_frequency index ~min_block_size ~matcher
  in
  let total_blocks =
    List.sum
      (module Int)
      matching_equivalences
      ~f:(fun equivalence -> Index.frequency_exn index equivalence)
  in
  printf "There are a total of %d blocks matching the query criteria.\n%!"
    total_blocks;
  let on_block _block ~file:_ ~equivalence:_ ~frequency:_ ~fundecl:_ =
    `Stop
  in
  let on_finish_iteration () = () in
  { on_block; on_finish_iteration }
;;
