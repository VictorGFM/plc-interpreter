(* PlcChecker *)

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


fun teval (e:expr, p:env) : plcType = 
    case e of
      ConI _ => IntT 
    | ConB _ => BoolT 
    | ESeq (SeqT t) => SeqT t 
    | ESeq (SeqT t) => SeqT t 
