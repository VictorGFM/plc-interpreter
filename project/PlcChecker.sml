(* PlcChecker *)

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

fun teval(e:expr, env:plcType env):plcType = 
    case e of
      Var(varName) => lookup env varName
    | ConI(_) => IntT 
    | ConB(_) => BoolT 
    | List([]) => ListT([])
    | List(l) => ListT(map (fn (item) => teval(item, env)) l)
    | ESeq (SeqT t) => SeqT(t)
    | Let(varName, expr1, expr2) => 
      let
        val type1 = teval(expr1, env)
        val type2 = teval(expr2, ((varName, type1)::env))
      in
        type2
      end
    | Letrec(funName, argsType, argsName, returnType, expr1, expr2) => 
      let
        val envFun = (funName, FunT(argsType, returnType))
        val envArgs = (argsName, argsType)
        val type1 = teval(expr1, (envFun::envArgs::env))
        val type2 = teval(expr2, (envFun::env))
      in
        if(type1 = returnType) then type2 else raise WrongRetType
      end
    | Anon(argsType, argsName, expr1) =>
      let
        val type1 = teval(expr1, ((argsName, argsType)::env))
      in
        FunT(argsType, type1)
      end
    | Call(expr1, expr2) =>
      let
        val type1 = teval(expr1, env)
        val type2 = teval(expr2, env)
      in
        case type1 of
          FunT(argsType, returnType) => 
            if(type2 = argsType) then returnType else raise CallTypeMisM
          | _ => raise NotFunc
      end
    | If(expr1, expr2, expr3) =>
      let
        val type1 = teval(expr1, env)
        val type2 = teval(expr2, env)
        val type3 = teval(expr3, env)
      in
        if(type1 <> BoolT) 
          then raise IfCondNotBool 
          else 
            if(type2 <> type3) 
              then raise DiffBrTypes 
              else type3
      end
    (*| (*TODO:Match*) => *)
    | Prim1(operator, expr1) =>
      let
        val type1 = teval(expr1, env)
      in
        case operator of
          ("!") => if(type1 = BoolT) then BoolT else raise CallTypeMisM (*TODO: check if its the correct exception to be raised*)
        | ("-") => if(type1 = IntT) then IntT else raise CallTypeMisM
        | ("hd") => 
          let in
            case expr1 of
              ESeq(SeqT(t)) => raise EmptySeq
            | _ => 
              let in
                case type1 of
                  SeqT(t) => t
                | _ => raise CallTypeMisM
              end
          end
        | ("tl") => 
          let in
            case expr1 of
              ESeq(SeqT(t)) => raise EmptySeq
            | _ => 
              let in
                case type1 of
                  SeqT(t) => SeqT(t)
                | _ => raise CallTypeMisM
              end
          end
        | ("ise") => 
          let in
            case type1 of
              SeqT(t) => BoolT
            | _ => raise CallTypeMisM
          end
        | ("print") => ListT([])
        | _ => raise UnknownType
      end
    | Prim2(operator, expr1, expr2) =>
      let
        val type1 = teval(expr1, env)
        val type2 = teval(expr2, env)
      in
        case operator of
            ("&&") => if(type1 = type2) then BoolT else raise NotEqTypes
          | ("::") => if (type2 = SeqT(type1)) then SeqT(type1) else raise CallTypeMisM
          | ("+") => if(type1=type2 andalso type1=IntT) then IntT else raise CallTypeMisM
          | ("-") => if(type1=type2 andalso type1=IntT) then IntT else raise CallTypeMisM
          | ("*") => if(type1=type2 andalso type1=IntT) then IntT else raise CallTypeMisM
          | ("/") => if(type1=type2 andalso type1=IntT) then IntT else raise CallTypeMisM
          | ("<") => if(type1=type2 andalso type1=IntT) then BoolT else raise CallTypeMisM
          | ("<=") => if(type1=type2 andalso type1=IntT) then BoolT else raise CallTypeMisM
          | ("=") => if(type1 <> type2) then raise NotEqTypes else
            let in
              case type1 of
                BoolT => BoolT
              | IntT => BoolT
              | ListT([]) => BoolT
              | SeqT(seqType) =>
                  let in
                    case seqType of
                      BoolT => BoolT
                    | IntT => BoolT
                    | ListT([]) => BoolT
                    | _ => raise NotEqTypes
                  end
              | ListT(typeList) => 
                let
                  val list = map(fn(t) => 
                      case t of
                        BoolT => BoolT
                      | IntT => BoolT
                      | ListT([]) => BoolT
                      | _ => raise NotEqTypes) 
                    typeList
                in
                  BoolT
                end
              | _ => raise UnknownType
            end
          | ("!=") => if(type1 <> type2) then raise NotEqTypes else
            let in
              case type1 of
                BoolT => BoolT
              | IntT => BoolT
              | ListT([]) => BoolT
              | SeqT(seqType) =>
                let in
                  case seqType of
                    BoolT => BoolT
                  | IntT => BoolT
                  | ListT([]) => BoolT
                  | _ => raise NotEqTypes
                end
              | ListT(typeList) => 
                let
                  val list = map(fn(t) => 
                      case t of
                        BoolT => BoolT
                      | IntT => BoolT
                      | ListT([]) => BoolT
                      | _ => raise NotEqTypes) 
                    typeList
                in
                  BoolT
                end
              | _ => raise UnknownType
            end
          | (";") => type2
        | _ => raise UnknownType
      end
    | Item(index, expr1) =>
      let
        val type1 = teval(expr1, env)
      in
        case type1 of
          ListT(typeList) => 
            let
              val listSize = length(typeList)
            in
              if(index < listSize) then List.nth(typeList, index) else raise ListOutOfRange
            end
        | _ => raise OpNonList
      end
    | _ => raise UnknownType