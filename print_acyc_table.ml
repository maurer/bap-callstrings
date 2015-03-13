open Core_kernel.Std
open Bap.Std
open Program_visitor
open Call_sensitivity_graph

module DotLCSG = Graph.Graphviz.Dot(LCSG)

let print_acyc project =
  let acsg = ACSG.of_project project in
  let lcsg = LCSG.contract @@ LCSG.of_acsg acsg project.symbols in
  let k_sensitive = LCSG.to_table lcsg (-1) in
  String.Table.iter k_sensitive ~f:(fun ~key:f ~data:css ->
    Printf.printf "Function %s:\n" f;
    List.iter css ~f:(fun cs ->
      List.iter cs ~f:(fun (a, f) ->
        Printf.printf "%s\t:\t%s\n" (Addr.to_string a) f
      );
      print_endline "----------------"
    )
  );
  project;;

register print_acyc;;
