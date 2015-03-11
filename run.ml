(** A tool to automate testing  *)
open Core_kernel.Std
open OUnit2
(* module C = Call_sensitivity_graph *)
type 'a task = {
  name : string;
  env : string Array.t;
  output : string
}

(* let read_tree x = failwith "read the ground truth tree"
let read_table chan = C.decode_calltable @@ In_channal.input_all chan
*)
let print_tree = {
  name = "print_tree";
  output = "test/fgr.tree";
  env = [||]
}

let print_k_sensitivity_table = {
  name = "print_k_sensitivity_table";
  env = [|"k=2"|];
  output = "test/fgr.ktable"
}

let table2tree = {
  name = "table2tree";
  env = [|"table_filename=table.out"; "table_root=main"|];
  output = "test/fgr.tabletree"
}

(*
let check_tree gt res = assert_equal ~cmp:(fun x y -> x = y) pread "diff %s %s" gt res
let check_table gt res = assert_equal ~cmp:cmp_table gt res
*)
(* TODO: Add check *)
let check a b = assert_equal ~cmp:(fun a b -> true) a b

(** [run task check input ctxt] run test for a given task on a the
    [input] file and apply [check] to verify the output *)
let run task check input gt ctxt : unit =
  let output = task.output in
  assert_command ~env:(Array.append task.env @@ Unix.environment ()) ~ctxt "bap-objdump" [input; "-l"; task.name];
  check task.output gt

(** {3 Actual testing}  *)

let suite = "BAP Fun" >::: [
    "print_tree" >::: [
      "fgr.out" >:: run print_tree check "test/fgr.out" "test/fgr.tree"
    ];
    "print_k_sensitivity_table" >::: [
      "fgr.out" >:: run print_k_sensitivity_table check "test/fgr.out" "test/fgr.ktable"
    ];
    "table2tree" >::: [
      "fgr.out" >:: run table2tree check "test/fgr.out" "test/fgr.tabletree"
    ]
  ]

let () = run_test_tt_main suite
