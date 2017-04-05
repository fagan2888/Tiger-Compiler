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
      (* Add each inst as a node *)
      val nodeID = ref 0
      fun add_node (a, graph) = (nodeID:=(!nodeID)+1; G.addNode(graph,!nodeID,a))
      val graph = foldl add_node G.empty alist

      (* Create label map*)
      val _ = nodeID := 0
      fun make_labmap (a, labmap) = (nodeID:=(!nodeID)+1; case a of A.LABEL{assem,lab} => L.insert(labmap,Symbol.name(lab),!nodeID) | _ => labmap)
      val labmap = foldl make_labmap L.empty alist

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
          A.OPER{assem, dst, src, jump} => (case jump of NONE => next_edge graph | SOME(jlist) => label_edge (jlist,graph))
          | _ => next_edge graph)
      val graph' = foldl add_edge graph alist

      (* made def *)
      val _ = nodeID := 0
      fun make_def (a, map) = (nodeID:=(!nodeID)+1;
        case a of
          A.OPER{assem,dst,src,jump} => M.insert(map,!nodeID,dst)
          | A.MOVE{assem,dst,src} => M.insert(map,!nodeID,[dst])
          | _ => M.insert(map,!nodeID,[]))
      val def = foldl make_def M.empty alist

      (* make use *)
      val _ = nodeID := 0
      fun make_use (a, map) = (nodeID:=(!nodeID)+1;
        case a of
          A.OPER{assem,dst,src,jump} => M.insert(map,!nodeID,src)
          | A.MOVE{assem,dst,src} => M.insert(map,!nodeID,[src])
          | _ => M.insert(map,!nodeID,[]))
      val use = foldl make_use M.empty alist

      (* make ismove *)
      val _ = nodeID := 0
      fun make_move (a, map) = (nodeID:=(!nodeID)+1;
        case a of
          A.MOVE{assem,dst,src} => M.insert(map,!nodeID,true)
          | _ => M.insert(map,!nodeID,false))
      val move = foldl make_move M.empty alist
    in
      (F.FLOWGRAPH{control=graph',def=def,use=use,ismove=move},G.nodes(graph'))
    end
end
