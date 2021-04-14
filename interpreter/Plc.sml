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

(*Environ*)
val ExceptionMessageSymbolNotFound = "EXCEPTION: SYMBOL NOT FOUND!"

fun run(e:expr) : string = 
    let
        val exprType = teval e []
        val exprValue = eval e []
    in
        val2string(exprValue) ^ " : " ^ type2string(exprType)
    end
    handle EmptySeq => ExceptionMessageEmptySeq 
            | UnknownType => ExceptionMessageUnknownType
            | NotEqTypes => ExceptionMessageNotEqTypes
            | WrongRetType => ExceptionMessageWrongRetType
            | DiffBrTypes => ExceptionMessageDiffBrTypes
            | IfCondNotBool => ExceptionMessageIfCondNotBool
            | NoMatchResults => ExceptionMessageNoMatchResults
            | MatchResTypeDiff => ExceptionMessageMatchResTypeDiff
            | MatchCondTypesDiff => ExceptionMessageMatchCondTypesDiff
            | CallTypeMisM => ExceptionMessageCallTypeMisM
            | NotFunc => ExceptionMessageNotFunc
            | ListOutOfRange => ExceptionMessageListOutOfRange
            | OpNonList => ExceptionMessageOpNonList
            | Impossible => ExceptionMessageImpossible
            | HDEmptySeq => ExceptionMessageHDEmptySeq
            | TLEmptySeq => ExceptionMessageTLEmptySeq
            | ValueNotFoundInMatch => ExceptionMessageValueNotFoundInMatch
            | NotAFunc => ExceptionMessageNotAFunc
            | SymbolNotFound => ExceptionMessageSymbolNotFound