val cases =
  ((*Nil*)
    let val s = "()";
        val e = "() : Nil"
    in
        (s, e)
    end
  ) ::
  ((*Boolean*)
    let val s = "true";
        val e = "true : Bool"
    in
        (s, e)
    end
  ) ::
  ((*Integer*)
    let val s = "7";
        val e = "7 : Int"
    in
        (s, e)
    end
  ) ::
  ((*List*)
    let val s = "(1, 2)";
        val e = "(1, 2, ) : (Int, Int, )"
    in
        (s, e)
    end
  ) ::
  ((*List of three elements*)
    let val s = "((), true, 7)";
        val e = "((), true, 7, ) : (Nil, Bool, Int, )"
    in
        (s, e)
    end
  ) ::
  ((*Named Function with one argument*)
    let val s = "fun test (Bool x) = !x; test";
        val e = "<fun> : Bool -> Bool"
    in
        (s, e)
    end
  ) ::
  ((*Named Function with two arguments*)
    let val s = "fun test (Int x, Int y) = x + y; test";
        val e = "<fun> : (Int, Int, ) -> Int"
    in
        (s, e)
    end
  ) ::
  ((*Empty Sequence*)
    let val s = "([Bool] [])";
        val e = "[] : [Bool]"
    in
        (s, e)
    end
  ) ::
  ((*Sequence with four elements*)
    let val s = "(1::7::5::2::([Int] []))";
        val e = "[1, 7, 5, 2, ] : [Int]"
    in
        (s, e)
    end
  ) ::
  ((*Boolean expression*)
    let val s = "3+1 = 4 && 4 <= 3";
        val e = "false : Bool"
    in
        (s, e)
    end
  ) ::
  ((*Head of a sequence*)
    let val s = "hd (1 :: 2 :: ([Int] []))";
        val e = "1 : Int"
    in
        (s, e)
    end
  ) ::
  ((*If with different branch types*)
    let val s = "if 3 = 2 then 7 else false";
        val e = ExceptionMessageDiffBrTypes
    in
        (s, e)
    end
  ) ::
  ((*Head of empty sequence*)
    let val s = "hd (([Int] []))";
        val e = ExceptionMessageEmptySeq
    in
        (s, e)
    end
  ) ::
  (* ((*Match without condition*)
    let val s = "var x = 2; match x with end";
        val e = "Exception"
    in
        (s, e)
    end
  ) :: *)
  (* ((*Match with different codition types*)
    let val s = "var x = 2; match x with | 7 -> 1 | false -> -1 end";
        val e = "Exception"
    in
        (s, e)
    end
  ):: *)
  (* ((*Match with different result types*)
    let val s = "var x = 2; match x with | 0 -> 1 | 1 -> true end";
        val e = "Exception"
    in
        (s, e)
    end
  ):: *)
  [ ((*Match with SOME and NONE*)
    let val s = "var x = 2; match x with | 0 -> 1 | _ -> -1 end";
        val e = "~1 : Int"
    in
        (s, e)
    end
  ) ];