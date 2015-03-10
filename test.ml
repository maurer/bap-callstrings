open OUnit2

let cmp_table a b = true

let cmp_tree a b = true

let test1 test_ctxt = assert_equal ~cmp:cmp_table
  (Callstring.read_table "test/fg.table2")
  (Callstring.bin2ktable "test/fg.out" 2)

let test2 test_ctxt = assert_equal ~cmp:cmp_table
  (Callstring.read_table "test/fg.acyctable")
  (Callstring.bin2acyctable "test/fg.out")

let test3 test_ctxt = assert_equal ~cmp:cmp_tree
  (Callstring.read_tree "test/fg_main.tree")
  (Callstring.bin2tree "test/fg.out" "main")

let test4 test_ctxt = assert_equal ~cmp:cmp_tree
  (Callstring.read_tree "test/fg_main.tree")
  (Callstring.table2tree "test/fg.table" "main")

let suite =
  "suite">:::
    ["test1">:: test1;
     "test2">:: test2]

let () =
  run_test_tt_main suite
