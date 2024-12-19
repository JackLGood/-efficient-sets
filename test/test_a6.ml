open OUnit2
open A6.Set
open QCheck

(* 88% coverage *)

module EquationalSpecificationOfSets = struct
  (** [make_is_empty_empty_test expected_output] tests that an empty set is
      indeed empty. *)
  let make_is_empty_empty_test expected_output =
    "1. is_empty empty = true" >:: fun _ ->
    assert_equal expected_output (is_empty empty)

  (** [make_is_empty_insert_test x expected_output] tests that inserting into a
      set makes it non-empty. *)
  let make_is_empty_insert_test x expected_output =
    "2. is_empty (insert x s) = false" >:: fun _ ->
    let inserted_tree = insert x empty in
    assert_equal expected_output (is_empty inserted_tree)

  (** [make_mem_empty_test x expected_output] tests that an element is not in an
      empty set. *)
  let make_mem_empty_test x expected_output =
    "3. mem x empty = false" >:: fun _ ->
    assert_equal expected_output (mem x empty)

  (** [make_mem_insert_same_test x expected_output] tests that inserting x makes
      x a member. *)
  let make_mem_insert_same_test x expected_output =
    "4. mem y (insert x s) = true if x = y" >:: fun _ ->
    assert_equal expected_output (mem x (insert x empty))

  (** [make_mem_insert_diff_test x y expected_output] tests that inserting x
      does not affect membership of a different y. *)
  let make_mem_insert_diff_test x y expected_output =
    "5. mem y (insert x s) = mem y s if x <> y" >:: fun _ ->
    assert_equal expected_output (mem y (insert x empty))
end

module SomeMoreCoverageTests = struct
  (** [make_insert_empty_test x v expected_tree] tests that inserting into an
      empty tree returns a valid and balanced tree. *)
  let make_insert_empty_test v expected_tree =
    "insert v into empty tree" >:: fun _ ->
    assert_equal ~printer:to_string_int_tree expected_tree (insert v empty)

  (** [make_insert_two_node v1 v2 expected_tree] tests that inserting into a two
      node tree makes it non-empty. *)
  let make_insert_two_node v1 v2 expected_tree =
    "insert v into empty tree" >:: fun _ ->
    let actual_tree = insert v2 (insert v1 empty) in
    assert_equal ~printer:to_string_int_tree expected_tree actual_tree

  (** [make_insert_two_node v1 v2 v3 expected_tree] tests that inserting into a
      three node tree makes it non-empty. *)
  let make_insert_three_node v1 v2 v3 expected_tree =
    "insert v into empty tree" >:: fun _ ->
    let actual_tree = insert v3 (insert v2 (insert v1 empty)) in
    assert_equal ~printer:to_string_int_tree expected_tree actual_tree

  (** [make_mem_three_node_lt x expected_output] tests getting mem of three node
      tree leads to no issues. *)
  let make_mem_three_node_lt x expected_output =
    "mem of three node" >:: fun _ ->
    assert_equal expected_output (mem x (insert 2 (insert 1 empty)))

  (** [generate_random_list] is an int list of size randomly 1-20 with values
      0-99. *)
  let generate_random_list =
    (* Random size between 1 and 20 *)
    let rec generate_list n acc =
      if n = 0 then acc else generate_list (n - 1) (Random.int 99 :: acc)
      (* Random integer between 1 and 100 *)
    in
    generate_list (Random.int 20) []

  (** [make_insert_random expected_output] tests 50 randmly generated int lists
      turns them into a 2-3 tree and checks their mem. *)
  let make_insert_random expected_output =
    "insert random numbers and check membership" >:: fun _ ->
    let repeat_count = 50 in
    List.iter
      (fun _ ->
        (* Generate a new list of random numbers each time *)
        let random_numbers = generate_random_list in
        (* Insert the random numbers into the set *)
        let s =
          List.fold_left (fun acc x -> insert x acc) empty random_numbers
        in
        (* Check that all numbers are present in the set *)
        List.iter
          (fun x -> assert_equal expected_output (mem x s))
          random_numbers)
      (List.init repeat_count (fun _ -> ()))
end

let test_suite =
  "2-3 Tree Set Tests"
  >::: [
         (* Testing Equation 1: is_empty empty = true *)
         EquationalSpecificationOfSets.make_is_empty_empty_test true;
         (* Testing Equation 2: is_empty (insert x s) = false *)
         EquationalSpecificationOfSets.make_is_empty_insert_test 5 false;
         (* Testing Equation 3: mem x empty = false *)
         EquationalSpecificationOfSets.make_mem_empty_test 3 false;
         (* Testing Equation 4: mem y (insert x s) = true if x = y *)
         EquationalSpecificationOfSets.make_mem_insert_same_test 7 true;
         (* Testing Equation 5: mem y (insert x s) = mem y s if x <> y *)
         EquationalSpecificationOfSets.make_mem_insert_diff_test 3 4 false;
         SomeMoreCoverageTests.make_mem_three_node_lt 1 true;
         SomeMoreCoverageTests.make_mem_three_node_lt 2 true;
         SomeMoreCoverageTests.make_insert_random true;
       ]

let () = run_test_tt_main test_suite
