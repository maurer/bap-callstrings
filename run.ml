(** A tool to automate testing  *)
open Core_kernel.Std
open OUnit2

type 'a task = {
  name : string;
  read : Sexp.t -> 'a;
}

let output_of_task task = task.name ^ ".scm"

let read_kstring _  = failwith "parse k-string"
let read_astring _  = failwith "parse a-string"
let read_treenode _ = failwith "parse a node"

let kstrings = {
  name = "kstrings";
  read = read_kstring;
}

let astrings = {
  name = "astrings";
  read = read_astring;
}

let csforest = {
  name = "csforest";
  read = read_treenode;
}

(** [run task check input ctxt] run test for a given task on a the
    [input] file and apply [check] to verify the output *)
let run task check input ctxt : unit =
  let output = output_of_task task in
  assert_command  ~ctxt "bap-objdump" [input; "-l"; task.name];
  In_channel.with_file output ~f:(fun chan ->
      Sexp.scan_sexps  (Lexing.from_channel chan)  |>
      List.map ~f:task.read |>  check)

(** {3 Actual testing}  *)

let dummy data = todo "test data"

let suite = "BAP Fun" >::: [
    "csforest" >::: [
      "dummy" >:: run kstrings dummy "filename"
    ]
  ]

let () = run_test_tt_main suite
