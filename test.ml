(** A tool to automate testing  *)
open Core_kernel.Std
open OUnit2
type 'a task = {
  name : string;
  env : string Array.t;
  output : string;
  dumptbl : bool;
  stdout : bool
}

let print_tree = {
  name = "print_tree";
  output = "graph.dot";
  env = [||];
  dumptbl = false;
  stdout = false;
}

let print_acyc_table = {
  name = "print_acyc_table";
  env = [||];
  output = "/tmp/fgr.acyctable";
  dumptbl = false;
  stdout = true;
}

let print_k_table = {
  name = "print_k_table";
  env = [|"k=2"|];
  output = "/tmp/fgr.ktable";
  dumptbl = false;
  stdout = true;
}

let table2tree = {
  name = "table2tree";
  env = [|"k=2"; "table_filename=table.out"; "table_root=main"|];
  output = "graph.dot";
  dumptbl = true;
  stdout = false;
}

let check a b = assert_equal ~cmp:(fun a b ->
  let cmd = Printf.sprintf "diff %s %s" a b in
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_all inp in
  r = ""
) a b

(** [run task check input ctxt] run test for a given task on a the
    [input] file and apply [check] to verify the output *)
let run task check input gt ctxt : unit =
  (if task.dumptbl then
    assert_command
      ~env:(Array.append task.env @@ Unix.environment ())
      ~ctxt
      "bap-objdump" [input; "-l"; "dump_k_table"]);
  assert_command
    ~foutput:(fun c_stream -> if task.stdout then
      Out_channel.with_file task.output ~f:(fun oc ->
        Stream.iter (Out_channel.output_char oc) c_stream; Out_channel.close oc))
    ~env:(Array.append task.env @@ Unix.environment ())
    ~ctxt
    "bap-objdump" [input; "-l"; task.name];
  check task.output gt

let suite = "BAP Fun" >::: [
    "print_tree" >::: [
      "fgr.out" >:: run print_tree check "test/fgr.out" "test/fgr.tree"
    ];
    "print_k_table" >::: [
      "fgr.out" >:: run print_k_table check "test/fgr.out" "test/fgr.ktable"
    ];
    "table2tree" >::: [
      "fgr.out" >:: run table2tree check "test/fgr.out" "test/fgr.tabletree"
    ];
    "print_acyc_table" >::: [
      "fgr.out" >:: run print_acyc_table check "test/fgr.out" "test/fgr.acyctable"
    ]
  ]

let () = run_test_tt_main suite
