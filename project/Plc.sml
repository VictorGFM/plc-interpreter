(* Plc interpreter main file *)

(*Type Checker Exceptions Messages*)
val ExceptionMessageEmptySeq = "EXCEPTION: INPUT SEQUENCE IS EMPTY!"
val ExceptionMessageUnknownType = "EXCEPTION: UNKNOWN EXCEPTION!"
val ExceptionMessageNotEqTypes = "EXCEPTION: COMPARISON WITH NON EQUAL TYPES!"
val ExceptionMessageWrongRetType = "EXCEPTION: FUNCTION RETURNING WRONG TYPE!"
val ExceptionMessageDiffBrTypes = "EXCEPTION: CONDITION WITH DIFFERENT BRANCH TYPES!";
val ExceptionMessageIfCondNotBool = "EXCEPTION: CONDITION IS NOT A BOOLEAN TYPE!"
val ExceptionMessageNoMatchResults = "EXCEPTION: NO RESULTS FOR MATCH EXPRESSION!"
val ExceptionMessageMatchResTypeDiff = "EXCEPTION: MATCH RETURNING DIFFERENT RESULT TYPES!"
val ExceptionMessageMatchCondTypesDiff = "EXCEPTION: MATCH EXPRESSION TYPE DIFFER FROM CONDITION TYPE!"
val ExceptionMessageCallTypeMisM = "EXCEPTION: WRONG ARGUMENT TYPE ON FUNCTION CALL!"
val ExceptionMessageNotFunc = "EXCEPTION: TRYING TO CALL A NON FUNCTION TYPE! (TYPE CHECKER)"
val ExceptionMessageListOutOfRange = "EXCEPTION: LIST INDEX OUT OF RANGE!"
val ExceptionMessageOpNonList = "EXCEPTION: TRYING TO ACCESS INDEX OF A NON LIST EXPRESSION!"

(*Interpreter Exceptions Messages*)
val ExceptionMessageImpossible = "EXCEPTION: IMPOSSIBLE EXCEPTION!"
val ExceptionMessageHDEmptySeq = "EXCEPTION: TRYING TO ACCESS HEAD OF EMPTY SEQUENCE!"
val ExceptionMessageTLEmptySeq = "EXCEPTION: TRYING TO ACCESS TAIL OF EMPTY SEQUENCE!"
val ExceptionMessageValueNotFoundInMatch = "EXCEPTION: VALUE NOT FOUND IN MATCH EXPRESSION!"
val ExceptionMessageNotAFunc = "EXCEPTION: TRYING TO CALL A NON FUNCTION TYPE! (INTERPRETER)"

fun run(e:expr) : string = 
    let
        val exprType = let in teval(e, []) end
        handle EmptySeq => let val p = print(ExceptionMessageEmptySeq^"\n") in raise EmptySeq end
            | UnknownType => let val p = print(ExceptionMessageUnknownType^"\n") in raise UnknownType end
            | NotEqTypes => let val p = print(ExceptionMessageNotEqTypes^"\n") in raise NotEqTypes end
            | WrongRetType => let val p = print(ExceptionMessageWrongRetType^"\n") in raise WrongRetType end
            | DiffBrTypes => let val p = print(ExceptionMessageDiffBrTypes^"\n") in raise DiffBrTypes end
            | IfCondNotBool => let val p = print(ExceptionMessageIfCondNotBool^"\n") in raise IfCondNotBool end
            | NoMatchResults => let val p = print(ExceptionMessageNoMatchResults^"\n") in raise NoMatchResults end
            | MatchResTypeDiff => let val p = print(ExceptionMessageMatchResTypeDiff^"\n") in raise MatchResTypeDiff end
            | MatchCondTypesDiff => let val p = print(ExceptionMessageMatchCondTypesDiff^"\n") in raise MatchCondTypesDiff end
            | CallTypeMisM => let val p = print(ExceptionMessageCallTypeMisM^"\n") in raise CallTypeMisM end
            | NotFunc => let val p = print(ExceptionMessageNotFunc^"\n") in raise NotFunc end
            | ListOutOfRange => let val p = print(ExceptionMessageListOutOfRange^"\n") in raise ListOutOfRange end
            | OpNonList => let val p = print(ExceptionMessageOpNonList^"\n") in raise OpNonList end

        val exprValue = let in eval(e, []) end 
        handle Impossible => let val p = print(ExceptionMessageImpossible^"\n") in raise Impossible end
            | HDEmptySeq => let val p = print(ExceptionMessageHDEmptySeq^"\n") in raise HDEmptySeq end
            | TLEmptySeq => let val p = print(ExceptionMessageTLEmptySeq^"\n") in raise TLEmptySeq end
            | ValueNotFoundInMatch => let val p = print(ExceptionMessageValueNotFoundInMatch^"\n") in raise ValueNotFoundInMatch end
            | NotAFunc => let val p = print(ExceptionMessageNotAFunc^"\n") in raise NotAFunc end
    in
        val2string(exprValue) ^ " : " ^ type2string(exprType)
    end