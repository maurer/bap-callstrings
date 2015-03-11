open Core_kernel.Std
open Bap.Std
open Program_visitor

module CallSite = struct
  include Addr
  let default = Addr.zero 0 (* Should never be used *)
end

(* Address Call Sensitivity Graph *)
module ACSG = struct
  module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Addr)(CallSite)
  include G
  let add_call (csg : t) (caller : Addr.t) (callee : Addr.t) (site : CallSite.t) : t =
    G.add_edge_e (G.add_vertex (G.add_vertex csg caller) callee) (caller, site, callee)
  let add_func (csg : t) (mem : mem) (disasm : disasm) : t =
    Seq.fold (Table.intersections (Disasm.blocks disasm) mem) ~init:csg ~f:(fun csg (mem, block) ->
      let func_id = Memory.min_addr mem in
      (* If there is to be a call, it must be the terminator *)
      (* I cannot use Block.terminator to access it because I need the memory address as well *)
      (* Why is there no Seq.last? We just don't know! *)
      let (insn_mem, insn) = List.hd_exn @@ Seq.to_list_rev @@ Block.insns block in
      if Insn.is_call insn
        then Seq.fold (Block.dests block) ~init:csg ~f:(fun csg dest ->
               match dest with
                 (* Resolved jump or conditional jump in a call insn is assumed a call target *)
                 | `Block (tgt, `Jump)
                 | `Block (tgt, `Cond) ->
                   add_call csg func_id (Block.addr tgt) (Memory.min_addr insn_mem)
                 (* Anything else is unresolved or a fallthrough, ignore it *)
                 | _ -> csg
             )
        else csg
    )
  let of_project (project : project) : t =
    (* We assume all functions discoverable via rec-cfg are in the symtable *)
    Table.foldi project.symbols ~init:empty ~f:(fun mem _ csg ->
      add_func csg mem @@ project.program
    )
end

(*
module Calltable = struct
  include String.Table
  type t = (addr * string) list list String.Table.t
end
*)

module Calltable = String.Table

(* Label Call Sensitivity Graph *)
module LCSG = struct
  module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(String)(CallSite)
  include G

  let edge_attributes (_, c, _) =
    [Graph.Graphviz.DotAttributes.(`Label (Addr.to_string c))]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = []
  let vertex_name x = x
  let default_vertex_attributes _ = []
  let graph_attributes _ = []


  let of_acsg (acsg : ACSG.t) (xlat_tab : String.t Table.t) : t =
    let xlat (addr : Addr.t) : String.t option =
      Option.map ~f:snd @@ Table.find_addr xlat_tab addr
    in
    ACSG.fold_edges_e (fun (s, c, d) csg ->
      match (xlat s, xlat d) with
        | (Some s', Some d') -> G.add_edge_e csg (s', c, d')
        | _ -> csg) acsg @@
      ACSG.fold_vertex (fun v csg ->
        match xlat v with
          | Some v' -> G.add_vertex csg v'
          | None -> csg) acsg G.empty

  let to_table (lcsg : t) (k : int) : (addr * string) list list Calltable.t =
    let rec step_down k v : (addr * string) list list =
      if (k = 0) || (0 = G.in_degree lcsg v) then [[]]
      else
        G.fold_pred_e (fun (s, l, _) acc ->
          List.rev_append
            (List.map (step_down (k - 1) s) ~f:(fun p -> (l, s) :: p)) acc
        ) lcsg v []
    in
    let table = Calltable.create () in
    G.iter_vertex (fun v ->
      Calltable.add_exn table ~key:(vertex_name v) ~data:(step_down k v)
    ) lcsg;
    table
end


let main project =
  let acsg = ACSG.of_project project in
  let lcsg = LCSG.of_acsg acsg project.symbols in
  let module Dot = Graph.Graphviz.Dot(LCSG) in
  Out_channel.with_file "graph.dot" ~f:(fun out ->
    Dot.output_graph out lcsg
  );
  project

let () = register main
