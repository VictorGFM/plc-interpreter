(* PlcInterp *)

exception Impossible 
exception HDEmptySeq 
exception TLEmptySeq 
exception ValueNotFoundInMatch 
exception NotAFunc 

fun eval (e:expr, env:plcVal env) : plcVal = 
    case e of
      Var(varName) => lookup env varName
    | ConI(n) => IntV(n) 
    | ConB(b) => BoolV(b) 
    | List([]) => ListV([])
    | List(l) => ListV(map (fn (item) => eval(item, env)) l)
    | ESeq (SeqT t) => SeqV([])
    | Let(varName, expr1, expr2) => 
      let
        val value1 = eval(expr1, env)
      in
        eval(expr2, ((varName, value1)::env))
      end
    | Letrec(funName, argsType, argsName, returnType, expr1, expr2) =>
      let 
        val closureEnv = (funName, Clos(funName, argsName, expr1, env))::env
      in
        eval(expr2, closureEnv)
      end
    | Anon(argsType, argsName, expr1) => Clos("", argsName, expr1, env) (*TODO*)
    | Call(expr1, expr2) =>
      let
        val value1 = eval(expr1, env)
      in
        case value1 of
          Clos(funName, argsName, funExpr, closEnv) => 
            let
              val value2 = eval(expr2, env)
              val funEnv = (funName, value1)::(argsName, value2)::closEnv
            in
              eval(funExpr, funEnv)
            end
          | _ => raise NotAFunc
      end
    | If(expr1, expr2, expr3) =>
      let
        val value1 = eval(expr1, env)
      in
        case value1 of
          BoolV(b) => if b then eval(expr2, env) else eval(expr3, env)
        | _ => raise Impossible
      end
    | Match (expr, matchExpr) =>
      if(matchExpr = []) then raise Impossible 
      else 
        let
          val exprValue = eval(expr, env)
          val matchCond = fn (condExpr, resultExpr) => ( 
            case condExpr of
              NONE => true
            | SOME(e) => 
              let
                val condValue = eval(e, env)
              in
                (condValue = exprValue)
              end
          )
          val filteredList = List.filter matchCond matchExpr
        in
          if filteredList = [] 
          then raise ValueNotFoundInMatch 
          else 
            let 
              val matchedExpr = hd(filteredList)
              val resultExpr = (#2 matchedExpr)
            in
              eval(resultExpr, env)
            end
        end
    | Prim1(operator, expr1) =>
      let
        val value1 = eval(expr1, env)
      in
        case operator of
          ("!") => let in
            case value1 of
              BoolV(b) => BoolV(not(b))
            | _ => raise Impossible
          end
        | ("-") => let in
            case value1 of
              IntV(n) => IntV(~n)
            | _ => raise Impossible
          end
        | ("hd") => let in
            case value1 of
              SeqV(s) => if (s = []) then raise HDEmptySeq else hd(s)
            | _ => raise Impossible
          end
        | ("tl") => let in
            case value1 of
              SeqV(s) => if (s = []) then raise TLEmptySeq else SeqV(tl(s))
            | _ => raise Impossible
          end
        | ("ise") => let in
            case value1 of
              SeqV(s) => BoolV(s = [])
            | _ => raise Impossible
          end
        | ("print") => 
          let
            val p = print(val2string(value1)^"\n")
          in
            ListV([])
          end
        | _ => raise Impossible
      end
    | Prim2(operator, expr1, expr2) =>
      let
        val value1 = eval(expr1, env)
        val value2 = eval(expr2, env)
      in
        case operator of 
            ("&&") => let in
              case (value1, value2) of
                (BoolV(b1), BoolV(b2)) => BoolV(b1 andalso b2)
              | _ => raise Impossible
            end
          | ("::") => let in
              case (value1, value2) of
                (BoolV(b), SeqV(s)) => SeqV(BoolV(b)::s)
              | (IntV(n), SeqV(s)) => SeqV(IntV(n)::s)
              | (ListV(l), SeqV(s)) => SeqV(ListV(l)::s)
              | (SeqV(s1), SeqV(s2)) => SeqV(SeqV(s1)::s2)
              | (Clos(c), SeqV(s)) => SeqV(Clos(c)::s)
              | _ => raise Impossible
            end
          | ("+") => let in
              case (value1, value2) of
                (IntV(n1), IntV(n2)) => IntV(n1 + n2)
              | _ => raise Impossible
            end
          | ("*") => let in
              case (value1, value2) of
                (IntV(n1), IntV(n2)) => IntV(n1 * n2)
              | _ => raise Impossible
            end
          | ("-") => let in
              case (value1, value2) of
                (IntV(n1), IntV(n2)) => IntV(n1 - n2)
              | _ => raise Impossible
            end
          | ("/") => let in
              case (value1, value2) of
                (IntV(n1), IntV(n2)) => IntV(n1 div n2)
              | _ => raise Impossible
            end
          | ("<") => let in
              case (value1, value2) of
                (IntV(n1), IntV(n2)) => BoolV(n1 < n2)
              | _ => raise Impossible
            end
          | ("<=") => let in
              case (value1, value2) of
                (IntV(n1), IntV(n2)) => BoolV(n1 <= n2)
              | _ => raise Impossible
            end
          | ("=") => let in
              case (value1, value2) of
                (IntV(n1), IntV(n2)) => BoolV(n1 = n2)
              | (BoolV(b1), BoolV(b2)) => BoolV(b1 = b2)
              | (ListV(l1), ListV(l2)) => BoolV(l1 = l2)
              | (SeqV(s1), SeqV(s2)) => BoolV(s1 = s2)
              | _ => raise Impossible
            end
          | ("!=") => let in
              case (value1, value2) of
                (IntV(n1), IntV(n2)) => BoolV(n1 <> n2)
              | (BoolV(b1), BoolV(b2)) => BoolV(b1 <> b2)
              | (ListV(l1), ListV(l2)) => BoolV(l1 <> l2)
              | (SeqV(s1), SeqV(s2)) => BoolV(s1 <> s2)
              | _ => raise Impossible
            end
          | (";") => value2
        | _ => raise Impossible
      end
    | Item(index, expr1) =>
      let
        val value1 = eval(expr1, env)
      in
        case value1 of
          ListV(valueList) => 
            let
              val listSize = length(valueList)
            in
              if (index > 0 andalso index <= listSize) 
              then List.nth(valueList, (index-1)) 
              else raise ListOutOfRange
            end
        | _ => raise OpNonList
      end
    | _ => raise Impossible