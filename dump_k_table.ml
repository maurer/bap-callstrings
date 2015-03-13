open Core_kernel.Std
open Bap.Std
open Program_visitor
open Call_sensitivity_graph

module DotLCSG = Graph.Graphviz.Dot(LCSG)

let k = int_of_string @@ Sys.getenv "k"

let print_k_sensitive project =
  let acsg = ACSG.of_project project in
  let lcsg = LCSG.of_acsg acsg project.symbols in
  let k_sensitive = LCSG.to_table lcsg k in
  Out_channel.write_all "table.out" ~data:(Calltable.encode k_sensitive);
  project;;

register print_k_sensitive;;
