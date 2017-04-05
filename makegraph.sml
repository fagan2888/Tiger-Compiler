structure G = FuncGraph(type ord_key = int; val compare = Int.compare)
structure M = SplayMapFn(type ord_key = int; val compare = Int.compare)
structure L = SplayMapFn(type ord_key = string; val compare = String.compare)
structure A = Assem

structure F =
struct
datatype flowgraph = FLOWGRAPH of {control: A.instr G.graph, def: Temp.temp list M.map, use: Temp.temp list M.map, ismove: bool M.map}
end

signature MAKEGRAPH =
sig
    val instrs2graph : A.instr list -> F.flowgraph * A.instr G.node list
end

structure makegraph : MAKEGRAPH =
struct
  fun instrs2graph alist =
    let
      (* Add each inst as a node and create label map *)
      val nodeID = ref 0
      fun add_node (a, (graph, label)) =
        (nodeID:=(!nodeID)+1;
        (G.addNode(graph,!nodeID,a), case a of A.LABEL{assem,lab} => L.insert(label,Symbol.name(lab),!nodeID) | _ => label))
      val (graph, labmap) = foldl add_node (G.empty,L.empty) alist

      (* create edges *)
      val lastNode = nodeID
      val _ = nodeID := 0
      fun next_edge graph = if (!nodeID)=(!lastNode) then graph else G.addEdge(graph, {from=(!nodeID), to=(!nodeID)+1})
      fun label_edge ([], graph) = graph
        | label_edge (lab::list, graph) = case L.find(labmap, Symbol.name(lab)) of
          SOME(labID) => label_edge (list, G.addEdge(graph,{from=(!nodeID), to=labID}))
          | NONE => label_edge (list,graph)
      fun add_edge (a, graph) = (nodeID:=(!nodeID)+1;
        case a of
          A.OPER{assem, src, dst, jump} => (case jump of NONE => next_edge graph | SOME(jlist) => label_edge (jlist,graph))
          | _ => next_edge graph)
      val graph' = foldl add_edge graph alist

      fun make_def (a, map) = map (* TODO *)
      val def = foldl make_def M.empty alist

      fun make_use (a, map) = map (* TODO *)
      val use = foldl make_use M.empty alist

      fun make_move (a, map) = map (* TODO *)
      val move = foldl make_move M.empty alist
    in
      (F.FLOWGRAPH{control=graph',def=def,use=use,ismove=move},G.nodes(graph'))
    end
end
