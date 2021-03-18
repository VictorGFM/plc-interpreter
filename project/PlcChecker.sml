(* PlcChecker *)

use "Environ.sml";

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList


fun teval(e:expr, p:env) : plcType = 
    case e of
    | []
      Var x => teval (lookup p x) p 
    | ConI _ => IntT 
    | ConB _ => BoolT 
    | List [] => ListT([])
    | List h::t => ListT(teval(h)::teval(t)::[])
    | ESeq (SeqT t) => SeqT(t)
    | Let (x, expr, expr)
    | (Prim2(op, e1, e2)) => 
      let
          val t1 = (teval e1 p);
          val t2 = (teval e2 p) 
      in
        case op of
          ("+") => if(t1=t2 andalso t1=IntT) then IntT else raise MatchResTypeDiff
        | ("-") => if(t1=t2 andalso t1=IntT) then IntT else raise MatchResTypeDiff
        | ("*") => if(t1=t2 andalso t1=IntT) then IntT else raise MatchResTypeDiff
        | ("/") => if(t1=t2 andalso t1=IntT) then IntT else raise MatchResTypeDiff
        | ("<") => if(t1=t2 andalso t1=IntT) then BoolT else raise MatchResTypeDiff
        | ("<=") => if(t1=t2 andalso t1=IntT) then BoolT else raise MatchResTypeDiff
        | ("=") => 
            if(t1=t2) 
            then 
              if(t1=BoolT orelse t1=IntT)
              then 
                t1
              else 
                if(t1=SeqT)
            else 
              raise MatchResTypeDiff

         (andalso t1=BoolT orelse ) then BoolT else MatchResTypeDiff
        | ("!=") =>
        | _ => raise NoMatchResults
      end


    | (Let(x, e1, e2)) => 
    let 
      t1 = teval e1 p
      t2 = teval e2 ((x, t1)::p)
    in 
      if (t1=t2) then t2  
    end


se x = bool BoolT else ConI
let val s = "var x = 4; x+1";
    val e = Let ("x",ConI 4,Prim2 ("+",Var "x",ConI 1))

if(op = "+" orelse op="-" orelse op="*" orelse op="/")
        then
          if()
        else
          raise
