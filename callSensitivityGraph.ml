open Core_kernel.Std
open Bap.Std
open Program_visitor

module FuncId = struct
  type t = string
  let compare = compare
  let equal = (=)
  let hash = Hashtbl.hash
end

module CallSite = struct
  type t = addr
  let compare = Addr.compare
  let equal = (=)
  let hash = Hashtbl.hash
  let default = Addr.zero 0 (* Should never be used *)
end

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

module LCSG(Label : Graph.Sig.COMPARABLE) = struct
  module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Label)(CallSite)
  include G
  let of_acsg (acsg : ACSG.t) (xlat_tab : Label.t Table.t) : t =
    let xlat (addr : Addr.t) : Label.t option =
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
end
