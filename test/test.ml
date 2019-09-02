open Core
open Instr_freq

let find_fixtures_directory () =
  let rec walk cur =
    if cur = Filename.root then
      failwith
        "Could not find fixtures directory! Make sure that you are running \
         the test from the project's root, or a subdirectory.";
    let attempt = cur ^/ "test" ^/ "fixtures" in
    match Sys.is_directory attempt with
    | `Yes -> attempt
    | `No | `Unknown -> walk (Filename.dirname cur)
  in
  walk (Filename.realpath Sys.executable_name)
;;

let matcher_for_move index =
  Index.Matcher.create
    [ {
        Index.With_register_information.desc =
          Index.Matcher.Basic (Types.From_cfg.Op Types.From_cfg.Move);
        arg = [| 0 |> Equivalence.Register_equivalence.of_int |];
        res = [| 1 |> Equivalence.Register_equivalence.of_int |];
      }
    ]
    index
;;

let build_index_for_fixture ~fixtures_directory ~context_length fixture_name
    =
  let ml_file = fixtures_directory ^/ sprintf "%s.ml" fixture_name in
  let open Async in
  (* Don't enter the async monad, to preserve error message call stacks, in
     case the tests fail. *)
  Thread_safe.block_on_async_exn (fun () ->
      Process.run_expect_no_output_exn () ~prog:"ocamlopt"
        ~args:
          [ "-save-ir-after";
            "linearize";
            "-stop-after";
            "linearize";
            ml_file
          ]);
  let linear_file = Filename.chop_extension ml_file ^ ".cmir-linear" in
  let blocks =
    Loop_free_block.read_file ~file:linear_file ~context_length
  in
  let index = Index.empty () in
  List.iter blocks ~f:(fun (_fundecl, loop_free_blocks) ->
      Array.iter loop_free_blocks ~f:(fun loop_free_block ->
          Index.update index loop_free_block));
  (blocks, index)
;;

let iter_blocks blocks ~index ~file ~statistics =
  let { Stats.on_block; on_finish_iteration } = statistics in
  List.iter blocks ~f:(fun (fundecl, loop_free_blocks) ->
      Array.iter loop_free_blocks ~f:(fun block ->
          let equivalence = Index.equivalence_exn index block in
          let frequency = Index.frequency_exn index equivalence in
          on_block block ~file ~equivalence ~frequency ~fundecl |> ignore));
  on_finish_iteration ()
;;

let test_index_statistics ~fixtures_directory =
  let _blocks, index =
    build_index_for_fixture "simple" ~fixtures_directory ~context_length:0
  in
  let { Index.n_symbolic_blocks;
        n_basic_instructions;
        n_terminator_instructions
      } =
    Index.hashtbl_load_statistics index
  in
  [%test_result: int] ~expect:8 n_symbolic_blocks;
  [%test_result: int] ~expect:11 n_basic_instructions;
  [%test_result: int] ~expect:4 n_terminator_instructions
;;

let test_matching_functionality ~fixtures_directory =
  printf "test_matching_functionality\n";
  let blocks, index =
    build_index_for_fixture "simple" ~fixtures_directory ~context_length:0
  in
  let matcher = Some (matcher_for_move index) in
  let statistics =
    Stats.combine
      [ Stats.count_blocks_matching index ~min_block_size:0 ~matcher;
        Stats.print_most_popular_classes index ~n_real_blocks_to_print:1
          ~n_most_frequent_equivalences:1
          ~block_print_mode:Loop_free_block.As_assembly ~min_block_size:0
          ~matcher
      ]
  in
  iter_blocks blocks ~index ~file:"simple.ml" ~statistics
;;

let test_index_bigger_file ~fixtures_directory =
  printf "test_index_bigger_file\n";
  let blocks, index =
    build_index_for_fixture "repetitive" ~fixtures_directory
      ~context_length:0
  in
  let statistics =
    Stats.combine
      [ Stats.print_most_popular_classes index ~n_real_blocks_to_print:1
          ~n_most_frequent_equivalences:1
          ~block_print_mode:Loop_free_block.As_cfg ~min_block_size:4
          ~matcher:None
      ]
  in
  iter_blocks blocks ~index ~file:"repetitive.ml" ~statistics
;;

let test_index_context ~fixtures_directory =
  printf "test_index_context\n";
  let blocks, index =
    build_index_for_fixture "repetitive" ~fixtures_directory
      ~context_length:1
  in
  let statistics =
    Stats.combine
      [ Stats.print_most_popular_classes index ~n_real_blocks_to_print:1
          ~n_most_frequent_equivalences:1
          ~block_print_mode:Loop_free_block.Both ~min_block_size:6
          ~matcher:None
      ]
  in
  iter_blocks blocks ~index ~file:"repetitive.ml" ~statistics
;;

let main ~fixtures_directory =
  test_index_statistics ~fixtures_directory;
  test_matching_functionality ~fixtures_directory;
  test_index_bigger_file ~fixtures_directory;
  test_index_context ~fixtures_directory
;;

let () = main ~fixtures_directory:(find_fixtures_directory ())
