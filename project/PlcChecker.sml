(* PlcChecker *)

use "Environ.sml";

exception EmptySeq (*v*)
exception UnknownType (*v*)
exception NotEqTypes (*v*)
exception WrongRetType (*v*)
exception DiffBrTypes (*v*)
exception IfCondNotBool (*v*)
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM (*v*)
exception NotFunc (*v*)
exception ListOutOfRange (*v*)
exception OpNonList (*v*)


fun teval(expr:expr, env: plcType env) : plcType = 
    case expr of
      Var(varName) => lookup(env, varName)
    | ConI(_) => IntT 
    | ConB(_) => BoolT 
    | List([]) => ListT([])
    | List(list) => map((fn(item) => teval(item, env)), list); 
    | ESeq (SeqT t) => SeqT(t)
    | Let(varName, expr1, expr2) => 
      let
        type1 = teval(expr1, env)
        type2 = teval(expr2, ((varName, type1)::env))
      in
        type2
      end
    | Letrec(funName, argsType, argsName, returnType, expr1, expr2) => 
      let
        envFun = (funName, FunT(argsType, returnType))
        envArgs = (argsName, argsType)
        type1 = teval(expr1, (envFun::envArgs::env))
        type2 = teval(expr2, envFun)
      in
        if(type1 = returnType) then type2 else raise WrongRetType
      end
    | Anon(argsType, argsName, expr1) =>
      let
        type1 = teval(expr1, ((argsName, argsType)::env))
      in
        FunT(argsType, type1)
      end
    | Call(expr1, expr2) =>
      let
        type1 = teval(expr1, env)
        type2 = teval(expr2, env)
      in
        case type2 of
          FunT(argsType, returnType) => 
            if(type1 = argsType) then returnType else raise CallTypeMisM
          | _ => raise NotFunc
      end
    | If(expr1, expr2, expr3) =>
      let
        type1 = teval(expr1, env)
        type2 = teval(expr2, env)
        type3 = teval(expr3, env)
      in
        if(type1 <> BoolT) 
          then raise IfCondNotBool 
          else 
            if(type2 <> type3) 
              then DiffBrTypes 
              else type3
      end
    | (*TODO:Match*) => 
    | Prim1(operator, expr1) =>
      let
        type1 = teval(expr1, env)
      in
        case operator of
          ("!") => if(type1 = BoolT) then BoolT else raise CallTypeMisM (*TODO: check if its the correct exception to be raised*)
        | ("-") => if(type1 = IntT) then IntT else raise CallTypeMisM
        | ("hd") => if(expr1 = ESeq(_)) then raise EmptySeq else 
          case type1 of
            SeqT(t) => t
          | _ => raise CallTypeMisM
        | ("tl") => if(expr1 = ESeq(_)) then raise EmptySeq else 
          case type1 of
            SeqT(t) => SeqT(t)
          | _ => raise CallTypeMisM
        | ("ise") =>
          case type1 of
            SeqT(t) => BoolT
          | _ => raise CallTypeMisM
        | ("print") => ListT([])
        | _ => raise UnknownType
      end
    | Prim2(operator, expr1, expr2) =>
      let
        type1 = teval(expr1, env)
        type2 = teval(expr2, env)
      in
        case operator of
            ("&&") => if(type1 = type2) then BoolT else raise NotEqTypes
          | ("::") => if (type2 = SeqT(type1)) then SeqT(type1) else raise CallTypeMisM
          | ("+") => if(type1=type2 andalso type1=IntT) then IntT else raise CallTypeMisM
          | ("-") => if(type1=type2 andalso type1=IntT) then IntT else raise CallTypeMisM
          | ("*") => if(type1=type2 andalso type1=IntT) then IntT else raise CallTypeMisM
          | ("/") => if(type1=type2 andalso type1=IntT) then IntT else raise CallTypeMisM
          | ("<") => if(type1=type2 andalso t1=IntT) then BoolT else raise CallTypeMisM
          | ("<=") => if(type1=type2 andalso t1=IntT) then BoolT else raise CallTypeMisM
          | ("=") => if(type1 <> type2) then raise NotEqTypes else
            case type1 of
              BoolT => BoolT
            | IntT => BoolT
            | ListT([]) => BoolT
            | SeqT(seqType) =>
              case seqType of
                BoolT => BoolT
              | IntT => BoolT
              | ListT([]) => BoolT
              | _ => raise NotEqTypes
            | ListT(typeList) => 
              map((fn(type) => 
                    case type of
                      BoolT => BoolT
                    | IntT => BoolT
                    | ListT([]) => BoolT
                    | _ => raise NotEqTypes), 
                  typeList);
            | _ => raise UnknownType
          | ("!=") => if(type1 <> type2) then raise NotEqTypes else
            case type1 of
              BoolT => BoolT
            | IntT => BoolT
            | ListT([]) => BoolT
            | SeqT(seqType) =>
              case seqType of
                BoolT => BoolT
              | IntT => BoolT
              | ListT([]) => BoolT
              | _ => raise NotEqTypes
            | ListT(typeList) => 
              map((fn(type) => 
                    case type of
                      BoolT => BoolT
                    | IntT => BoolT
                    | ListT([]) => BoolT
                    | _ => raise NotEqTypes), 
                  typeList);
            | _ => raise UnknownType
          | (";") => type2
        | _ => raise UnknownType
      end
    | Item(index, expr1) =>
      let
        type1 = teval(expr1, env)
      in
        case type1 of
          ListT(typeList) => 
            let
              listSize = length(typeList)
            in
              if(index < listSize) then typeList[index] else raise ListOutOfRange
            end
        | _ => raise OpNonList
      end
    | _ => UnknownType