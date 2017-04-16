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
  fun color {interference, initial, registers} =
    let
     (* TODO: complete *)
    in
      (initial,[])
    end
end
