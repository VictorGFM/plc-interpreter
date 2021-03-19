(* Plc interpreter main file *)

fun run(e:expr) = 
    let
        val exprType = teval(e, [])
    in
        type2string(exprType)
    end
   