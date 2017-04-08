structure M = SplayMapFn(type ord_key = int; val compare = Int.compare)
												
signature LIVENESS =
sig
  type igraph
  val interferenceGraph : Flow.flowgraph -> igraph * (Temp.temp Flow.Graph.node -> Temp.temp list)
  val show : TextIO.outstream * igraph -> unit
end

structure Liveness : LIVENESS =
struct

datatype igraph = IGRAPH of {graph: Temp.temp Flow.Graph.graph,
														 tnode: Temp.temp list M.map,
														 gtemp: Flow.Graph.node list M.map,
														 moves: (Temp.temp Flow.Graph.node * Temp.temp Flow.Graph.node) list}

fun interferenceGraph (fg as Flow.FLOWGRAPH{control,def,use,ismove}) =
	let
			val liveInMap = M.empty
			val liveOutMap = M.empty
													 
			(* Create Live-In and Live-Out maps *)
			fun compLiveness (inMap,outMap) =
				let
						fun concatList (list1,list2) = list1 @ list2
						fun filterList (list1,list2) =
							List.filter (fn x => List.all (fn y => x <> y) list2) list1
						val outDefMap = M.unionWith filterList (outMap, (#def fg))
						val inMap' = M.unionWith concatList ((#use fg), outDefMap)
						fun getOut node =
							let
									val succList = Flow.Graph.succs' (#control fg) node
									fun getLive id = M.find(inMap,id)
									val nodeID = Flow.Graph.getNodeID node
							in
									M.insert(outMap, nodeID, map getLive succList)
							end

						val nodeList = Flow.Graph.nodes (#control fg)
						val outMap' = foldl getOut M.empty nodeList
				in
						if (inMap = inMap' andalso outMap = outMap') then
								(inMap,outMap)
						else
								compLiveness(inMap',outMap')
				end

						(* TODO: Create Interference Graph nodes, each of which is a temp
Create Interference Graph edges by iterating through the flow nodes. Where there is a newly defined temp and existing temps are in the liveOut map, add an edge between them
Maybe figure out MOVE.
Also need to figure out gtemp and tnode in the igraph datatype *)

	in
			{}
	end
			

fun show (outstream, IGRAPH{graph,tnode,gtemp,moves}) =
		Flow.Graph.printGraph (fn (nodeID,temp) => "Node " ^ (Int.toString nodeID) ^ ": " ^ (Temp.makestring temp)) graph
end
