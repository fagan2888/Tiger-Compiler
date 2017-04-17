signature COLOR =
sig
  structure Frame : FRAME
  type allocation = Frame.register Temp.Map.map
  val color : {interference: Liveness.igraph,
              initial: allocation,
              registers: Frame.register list}
              -> allocation * Temp.temp list
end

structure Color : COLOR =
struct
  structure Frame = MipsFrame
  type allocation = Frame.register Temp.Map.map
  fun color {interference as Liveness.IGRAPH{graph, tnode, gtemp, moves}, initial, registers} =
    let
      fun precolored node = List.exists (fn listitem => listitem=Flow.Graph.nodeInfo(node)) (#1 (ListPair.unzip(Temp.Map.listItemsi(initial))))

      fun spill graph =
        let
          fun removeNode [] = (graph,List.hd(Flow.Graph.nodes graph),false)
            | removeNode (node::nodelist) = if not(precolored node) then (Flow.Graph.remove(graph,node), node, true) else removeNode(nodelist)
        in
          removeNode (Flow.Graph.nodes graph)
        end

      fun simplify graph =
        let
          fun removeNode [] = (graph,List.hd(Flow.Graph.nodes graph),false)
            | removeNode (node::nodelist) = if (not(precolored node) andalso Flow.Graph.degree(node) < List.length(registers)) then (Flow.Graph.remove(graph,node), node, true) else removeNode(nodelist)
        in
          removeNode (Flow.Graph.nodes graph)
        end

      (* Add all nodes non-precolored nodes to stack or spilled arrays *)
      fun createStack (graph, stack, spilled) =
        let
          val (graphsi, nodesi, didSimplify) = simplify graph
          val (graphsp, nodesp, didSpill) = spill graph
        in
          if didSimplify
          then createStack (graphsi, nodesi::stack, spilled)
          else (if didSpill then createStack (graphsp, stack, nodesp::spilled) else (stack, spilled))
        end

      val (stack, spilled) = createStack (graph, [], [])

     (* TODO: color stack -> add to initial map
              color spilled nodes - see if they actually spill *)
    in
      (initial,[])
    end
end
