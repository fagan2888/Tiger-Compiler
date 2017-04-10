(* structure M = SplayMapFn(type ord_key = int; val compare = Int.compare) *)
												
signature LIVENESS =
sig
  type igraph
  val interferenceGraph : Flow.flowgraph -> igraph (* * (Temp.temp Flow.Graph.node -> Temp.temp list) *)
  val show : TextIO.outstream * igraph -> unit
end

structure Liveness : LIVENESS =
struct

datatype igraph = IGRAPH of {graph: Temp.temp Flow.Graph.graph,
														 tnode: Temp.temp Flow.Graph.node Temp.Map.map,
														 gtemp: Temp.temp M.map (*,
														 moves: (Temp.temp Flow.Graph.node * Temp.temp Flow.Graph.node) list *)}

fun interferenceGraph (fg as Flow.FLOWGRAPH{control,def,use,ismove}) =
	let
			(*Concatenate two lists and remove duplicates*)
			fun concatList (list1,list2) =
				let
						fun removeDups [] = []
							| removeDups (x::xs) = x::removeDups(List.filter (fn y => y <> x) xs)
				in
						removeDups(list1 @ list2)
				end

			(* Create Live-In and Live-Out maps *)
			fun compLiveness (inMap,outMap) =
				let
						fun filterList (list1,list2) =
							List.filter (fn x => List.all (fn y => x <> y) list2) list1

						fun eqMap (map1, map2) =
							let
									val maplist1 = M.listItems(map1)
									val maplist2 = M.listItems(map2)
							in
									maplist1 = maplist2
							end
													
						val outDefMap = M.unionWith filterList (outMap, def)
						val inMap' = M.unionWith concatList (use, outDefMap)
																		 
						fun getOut (node,somemap) =
							let
									val succList = Flow.Graph.succs node
									fun getLive id =
										case M.find(inMap,id) of
												SOME(templist) => templist
											| NONE => []
									val nodeID = Flow.Graph.getNodeID node
							in
									M.insert(somemap, nodeID, List.concat(map getLive succList))
							end
									
						val nodeList = Flow.Graph.nodes control
						val outMap' = foldl getOut M.empty nodeList
				in
						if (eqMap(inMap,inMap') andalso eqMap(outMap,outMap')) then
								(inMap,outMap)
						else
								compLiveness(inMap',outMap')
				end

			val liveOutMap = #2 (compLiveness(M.empty, M.empty))

			(* Create Nodes of Igraph *)
      val nodeID = ref 0
			val tnode = Temp.Map.empty
			val templist = concatList(List.concat(M.listItems(use)),List.concat(M.listItems(def)))
      fun add_node (t, graph) = (nodeID:=(!nodeID)+1;
																 Flow.Graph.addNode(graph,!nodeID,t))
      val igraph = foldl add_node Flow.Graph.empty templist

			(* Make tnode *)
			val _ = nodeID:=0
			fun make_tnode (temp, map) = (nodeID:=(!nodeID)+1;
																		Temp.Map.insert(map,temp,Flow.Graph.getNode(igraph,!nodeID)))
			val tnode = foldl make_tnode Temp.Map.empty templist

			(* Make gtemp *)
			val _ = nodeID:=0
			fun make_gtemp (t, map) = (nodeID:=(!nodeID)+1;
																 M.insert(map,!nodeID,t))
			val gtemp = foldl make_gtemp M.empty templist

			(* Create Edges of IGraph *)
			val _ = nodeID:=1

			fun temptoNode t =
				Flow.Graph.getNodeID(
						case Temp.Map.find(tnode,t) of
								SOME(node) => node
							| NONE => Flow.Graph.getNode(Flow.Graph.empty,~1) (* should never happen *)
				)
													
			fun makeEdges (defs,graph) =
				let
						val liveouts =
								case M.find(liveOutMap,!nodeID) of
										SOME(templist) => templist
									| NONE => []
						val liveNodes = map temptoNode liveouts
						fun addEdges (graph, x::xs, y::ys) =
							let
									fun addEdge (graph, t1, t2::t2s) = addEdge(Flow.Graph.doubleEdge(graph,t1,t2),t1,t2s)
										| addEdge (graph, t1, []) = graph
							in
									addEdges(addEdge(graph,x,y::ys),xs,y::ys)
							end
							| addEdges (graph,[],y::ys) = graph
							| addEdges (graph,_,_) = graph
				in
						(nodeID:=(!nodeID)+1;
						addEdges(graph,defs,liveNodes))
				end

			val defNodes = M.map (map temptoNode) def
			val igraph' = M.foldl makeEdges igraph defNodes


(* TODO: Finish move *)

	in
			IGRAPH{graph=igraph',tnode=tnode,gtemp=gtemp(*,moves=[]*)}
	end
			

fun show (outstream, IGRAPH{graph,tnode,gtemp(*,moves*)}) =
		Flow.Graph.printGraph (fn (nodeID,temp) => "Node " ^ (Int.toString nodeID) ^ ": " ^ (Temp.makestring temp)) graph
end
