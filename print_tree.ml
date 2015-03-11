open Core_kernel.Std
open Bap.Std
open Program_visitor
open Call_sensitivity_graph

module DotTree = Graph.Graphviz.Dot(Tree)

let print_lcfg project =
  let acsg = ACSG.of_project project in
  let lcsg = LCSG.of_acsg acsg project.symbols in
  let tree = Tree.of_lcsg lcsg "main" in
  Out_channel.with_file "graph.dot" ~f:(fun out ->
    DotTree.output_graph out tree
  );
  project;;

register print_lcfg;;
