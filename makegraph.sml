structure M = SplayMapFn(type ord_key = int; val compare = Int.compare)
structure L = SplayMapFn(type ord_key = string; val compare = String.compare)
structure A = Assem

structure Flow =
struct
structure Graph = FuncGraph(type ord_key = int; val compare = Int.compare)
datatype flowgraph = FLOWGRAPH of {control: A.instr Graph.graph, def: Temp.Set.set M.map, use: Temp.Set.set M.map, ismove: bool M.map}
fun show (FLOWGRAPH{control,def,use,ismove}) =
  let
    fun strtlist ([],str) = str ^ "\n"
      | strtlist ([temp],str) = str ^ (Temp.makestring temp) ^ "\n"
      | strtlist (temp::list, str) = strtlist(list, str ^ (Temp.makestring temp) ^ ", ")
    val _ = print ("USE\n")
    val _ = M.appi (fn (id, templist) => print("Node " ^ (Int.toString id) ^ ": " ^ (strtlist (templist,"")))) (M.map Temp.Set.listItems use)
    val _ = print ("DEF\n")
    val _ = M.appi (fn (id, templist) => print("Node " ^ (Int.toString id) ^ ": " ^ (strtlist (templist,"")))) (M.map Temp.Set.listItems def)
    val _ = print ("CONTROL\n")
    val _ = Graph.printGraph (fn (id,instr) => "Node " ^ (Int.toString id)) control
  in
    ()
  end 
end

signature MAKEGRAPH =
sig
    val instrs2graph : A.instr list -> Flow.flowgraph * A.instr Flow.Graph.node list
end

structure Makegraph : MAKEGRAPH =
struct
  fun instrs2graph alist =
    let
      (* Add each inst as a node, keeps last node at the end *)
      val lastNode = ref 0
      fun add_node (a, graph) = (lastNode:=(!lastNode)+1; Flow.Graph.addNode(graph,!lastNode,a))
      val graph = foldl add_node Flow.Graph.empty alist

      (* Create label map*)
      val nodeID = ref 0
      fun make_labmap (a, labmap) = (nodeID:=(!nodeID)+1; case a of A.LABEL{assem,lab} => L.insert(labmap,Symbol.name(lab),!nodeID) | _ => labmap)
      val labmap = foldl make_labmap L.empty alist

      (* create edges *)
      val _ = nodeID := 0
      fun next_edge graph = if (!nodeID)=(!lastNode) then graph else Flow.Graph.addEdge(graph, {from=(!nodeID), to=(!nodeID)+1})
      fun label_edge ([], graph) = graph
        | label_edge (lab::list, graph) = case L.find(labmap, Symbol.name(lab)) of
          SOME(labID) => label_edge (list, Flow.Graph.addEdge(graph,{from=(!nodeID), to=labID}))
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
          A.OPER{assem,dst,src,jump} => M.insert(map,!nodeID,Temp.Set.addList(Temp.Set.empty,dst))
          | A.MOVE{assem,dst,src} => M.insert(map,!nodeID,Temp.Set.add(Temp.Set.empty,dst))
          | _ => M.insert(map,!nodeID,Temp.Set.empty))
      val def = foldl make_def M.empty alist

      (* make use *)
      val _ = nodeID := 0
      fun make_use (a, map) = (nodeID:=(!nodeID)+1;
        case a of
          A.OPER{assem,dst,src,jump} => M.insert(map,!nodeID,Temp.Set.addList(Temp.Set.empty,src))
          | A.MOVE{assem,dst,src} => M.insert(map,!nodeID,Temp.Set.add(Temp.Set.empty,src))
          | _ => M.insert(map,!nodeID,Temp.Set.empty))
      val use = foldl make_use M.empty alist

      (* make ismove *)
      val _ = nodeID := 0
      fun make_move (a, map) = (nodeID:=(!nodeID)+1;
        case a of
          A.MOVE{assem,dst,src} => M.insert(map,!nodeID,true)
          | _ => M.insert(map,!nodeID,false))
      val move = foldl make_move M.empty alist
    in
      (Flow.FLOWGRAPH{control=graph',def=def,use=use,ismove=move},Flow.Graph.nodes(graph'))
    end
end
