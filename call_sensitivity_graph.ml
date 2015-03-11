open Core_kernel.Std
open Sexplib.Std
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

type kcs = (addr * string) list list with sexp
let encode_calltable tab = Sexp.to_string (String.Table.sexp_of_t sexp_of_kcs tab)
let decode_calltable str = String.Table.t_of_sexp kcs_of_sexp @@ Sexp.of_string str

(* Label Call Sensitivity Graph *)
module LCSG = struct
  module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(String)(CallSite)
  include G
  let add_call (csg : t) (caller : string) (callee : string) (site : CallSite.t) : t =
    G.add_edge_e (G.add_vertex (G.add_vertex csg caller) callee) (caller, site, callee)

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

  let of_table (filename : string) : t =
    (* let table = readin filename in *)
    let ic = In_channel.create filename in
    let table = decode_calltable (In_channel.input_all ic) in
    (* let table = Calltable.create () in *)
    Calltable.fold table ~init:G.empty
      ~f:(fun ~key:(leaf : string) ~data:(ll : (addr * string) list list) g ->
        List.fold ll ~init:g ~f:(fun g' (l : (addr * string) list) ->
          let gg, _ = List.fold l ~init:(g', leaf) ~f:(fun (t, callee) (site, caller) ->
            (add_call t caller callee site), caller) in
          gg))

end

module Tree = struct
  type call = string * Addr.t with sexp
  module Node = struct
    module L = struct
      type t = E of call
             | T of call
             | R of (call * string)
             | Intermediate of call
             | Root of string
      with sexp

      let call_to_string (f, addr) =
        Printf.sprintf "%s:%s" f @@ Addr.to_string addr
      let to_string = function
        | E c -> Printf.sprintf "E(%s)" @@ call_to_string c
        | T c -> Printf.sprintf "T(%s)" @@ call_to_string c
        | R (c, f) -> Printf.sprintf "R(%s, %s)" (call_to_string c) f
        | Intermediate c -> Printf.sprintf "%s" @@ call_to_string c
        | Root f -> f
    end
    type l = L.t
    type t = (l * int)
    let alloc = ref 0
    let fresh (l : l) : t =
      alloc := !alloc + 1;
      (l, !alloc)
    let compare = compare
    let hash = Hashtbl.hash
    let equal = (=)
  end
  module G = Graph.Persistent.Digraph.ConcreteBidirectional(Node)
  include G
  let rec make_tree (lcsg : LCSG.t) (root : string) (t : t) (prefix : string list) : (t * G.V.t list) =
    let open Node in
    LCSG.fold_succ_e (fun (_, c, d) (t, vs) ->
      (* If the out degree is 0, it is a terminal node *)
      if ((LCSG.out_degree lcsg d) = 0)
        then let v = fresh @@ L.T(d, c) in
             (G.add_vertex t v, v::vs)
      (* If the a function the target calls is already on the stack, it is recursive *)
      else match List.find (LCSG.succ lcsg d) ~f:(List.mem prefix) with
             | Some r -> let v = fresh @@ L.R((d, c), r) in
                         (G.add_vertex t v, v::vs)
             | None ->
      (* It must be an intermediate node *)
      let (t', sub_vs) = make_tree lcsg d t (d::prefix) in
      let v = fresh @@ L.Intermediate(d, c) in
      (List.fold_left sub_vs ~f:(fun t v2 -> G.add_edge t v v2) ~init:(G.add_vertex t' v), v::vs)
    ) lcsg root (t, [])
  let of_lcsg (lcsg : LCSG.t) (root : string) : t =
    let (t, vs) = make_tree lcsg root (G.empty) [root] in
    let v = Node.fresh @@ Node.L.Root(root) in
    List.fold_left vs ~f:(fun t v2 -> G.add_edge t v v2) ~init:(G.add_vertex t v)
  module TS = Sexpable.To_stringable(Node.L)
  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes (x,_) = [`Label(Node.L.to_string x)]
  let vertex_name (_,n) = string_of_int n
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end
