open Core_kernel.Std
open Bap.Std
open Program_visitor
open Call_sensitivity_graph

module DotLCSG = Graph.Graphviz.Dot(LCSG)

let print_k_sensitive project =
  let acsg = ACSG.of_project project in
  let lcsg = LCSG.of_acsg acsg project.symbols in
  let k_sensitive = LCSG.to_table lcsg 2 in
  Out_channel.with_file "table.out" ~f:(fun oc ->
    Out_channel.output_string oc @@ encode_calltable k_sensitive
  );
  project;;

register print_k_sensitive;;
