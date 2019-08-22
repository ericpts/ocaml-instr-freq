open Core
open Ocamlcfg

type t = {
  on_block :
    Cfg.block ->
    equivalence:Index.Block_equivalence.t ->
    frequency:int ->
    [ `Stop | `Continue ];
  on_finish_iteration : unit -> unit;
}

let combine ts =
  let active_ts = ref ts in
  let on_block block ~equivalence ~frequency =
    active_ts :=
      List.filter !active_ts ~f:(fun t ->
          match t.on_block block ~equivalence ~frequency with
          | `Continue -> true
          | `Stop -> false);
    if List.length !active_ts > 0 then `Continue else `Stop
  in
  let on_finish_iteration () =
    List.iter ts ~f:(fun t -> t.on_finish_iteration ())
  in
  { on_block; on_finish_iteration }
;;

let print_most_popular_classes index ~n_most_frequent_equivalences
    ~max_representatives_per_equivalence ~block_print_mode ~min_block_size =
  let equivalences_to_print =
    List.take
      (Index.equivalences_by_frequency index ~min_block_size)
      n_most_frequent_equivalences
  in
  let remaining_to_print =
    match
      Hashtbl.create_mapped
        (module Index.Block_equivalence)
        ~get_key:Fn.id
        ~get_data:(Fn.const max_representatives_per_equivalence)
        equivalences_to_print
    with
    | `Ok hashtbl -> hashtbl
    | `Duplicate_keys list ->
        failwithf
          !"Received unexpected duplicated equivalence indices: %{sexp: \
            Index.Block_equivalence.t list} "
          list ()
  in
  let on_block block ~equivalence ~frequency =
    match Hashtbl.find remaining_to_print equivalence with
    | None -> `Continue
    | Some n ->
        (* XCR gyorsh for ericpts: can you finish iteration earlier here?
           just remove from equivalences_to_print when current_printed
           reaches max? I bet it will finish after scanning just a few of
           the linear files. *)
        printf "Equivalence %d with %d members: \n"
          (Index.Block_equivalence.to_int equivalence)
          frequency;
        Utils.print_block block ~block_print_mode;
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

let count_equivalence_classes_of_each_size () =
  let equivalence_classes_of_each_size = Hashtbl.create (module Int) in
  let on_block _block ~equivalence:_ ~frequency =
    Hashtbl.update equivalence_classes_of_each_size frequency ~f:(function
      | Some x -> x + 1
      | None -> 1);
    `Continue
  in
  let on_finish_iteration () =
    printf
      !"(numbers of members in equivalence class, how many equivalence \
        classes there are with this many members):\n\
        %{sexp: (int, int) Hashtbl.t}\n"
      equivalence_classes_of_each_size
  in
  { on_block; on_finish_iteration }
;;
