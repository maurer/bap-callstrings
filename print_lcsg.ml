open Core_kernel.Std
open Bap.Std
open Program_visitor
open Call_sensitivity_graph

module LCSG    = LCSG_Make(String)
module DotLCSG = Graph.Graphviz.Dot(LCSG)

let print_lcfg project =
  let acsg = ACSG.of_project project in
  let lcsg = LCSG.of_acsg acsg project.symbols in
  Out_channel.with_file "graph.dot" ~f:(fun out ->
    DotLCSG.output_graph out lcsg
  );
  project;;

register print_lcfg;;
