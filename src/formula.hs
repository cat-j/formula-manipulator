module Formula
    (Formula, negate)
where

data Formula
    = Lit String
    | Neg Formula
    | And Formula Formula
    | Or Formula Formula
    | Implies Formula Formula
    | Iff Formula Formula

foldFormula ::
    (String -> a) ->
    (a -> a) ->
    (a -> a -> a) ->
    (a -> a -> a) ->
    (a -> a -> a) ->
    (a -> a -> a) ->
    Formula -> a

foldFormula caseLit caseNeg caseAnd caseOr caseImplies caseIff aFormula =
    case aFormula of
        Lit name -> caseLit name
        Neg form -> caseNeg (rec form)
        And form1 form2 -> caseAnd (rec form1) (rec form2)
        Or form1 form2 -> caseOr (rec form1) (rec form2)
        Implies form1 form2 -> caseImplies (rec form1) (rec form2)
        Iff form1 form2 -> caseIff (rec form1) (rec form2)
    where rec = foldFormula caseLit caseNeg caseAnd caseOr caseImplies caseIff

recFormula ::
    (String -> a) ->
    (a -> Formula -> a) ->
    (a -> a -> Formula -> Formula -> a) ->
    (a -> a -> Formula -> Formula -> a) ->
    (a -> a -> Formula -> Formula -> a) ->
    (a -> a -> Formula -> Formula -> a) ->
    Formula -> a

recFormula caseLit caseNeg caseAnd caseOr caseImplies caseIff aFormula =
    case aFormula of
        Lit name -> caseLit name
        Neg form -> caseNeg (rec form) form
        And form1 form2 -> caseAnd (rec form1) (rec form2) form1 form2
        Or form1 form2 -> caseOr (rec form1) (rec form2) form1 form2
        Implies form1 form2 -> caseImplies (rec form1) (rec form2) form1 form2
        Iff form1 form2 -> caseIff (rec form1) (rec form2) form1 form2
    where rec = recFormula caseLit caseNeg caseAnd caseOr caseImplies caseIff

negate :: Formula -> Formula

negate = recFormula
    (Neg . Lit)
    (\_ form -> form)
    (\negatedForm1 negatedForm2 _ _ -> Or negatedForm1 negatedForm2)
    (\negatedForm1 negatedForm2 _ _-> And negatedForm1 negatedForm2)
    (\_ _ form1 form2 -> And form1 (Neg form2))
    (\_ _ form1 form2 -> Or (And form1 (Neg form2)) (And form2 (Neg form1)))