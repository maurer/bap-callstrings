open Core_kernel.Std
open Bap.Std
open Program_visitor
open Call_sensitivity_graph

module DotTree = Graph.Graphviz.Dot(Tree)

let filename = Sys.getenv "table_filename"
let root = Sys.getenv "table_root"

let print_lcfg project =
  let lcsg = LCSG.of_table filename in
  let tree = Tree.of_lcsg lcsg root in
  Out_channel.with_file "graph.dot" ~f:(fun out ->
    DotTree.output_graph out tree
  );
  project;;

register print_lcfg;;
