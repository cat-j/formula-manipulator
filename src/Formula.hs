module Formula
    (Formula, formLit, formNeg, formAnd, formOr, formImplies, formIff, negation)
where

data Formula
    = Lit String
    | Neg Formula
    | And Formula Formula
    | Or Formula Formula
    | Implies Formula Formula
    | Iff Formula Formula
    deriving (Eq, Show)

formLit :: String -> Formula
formLit = Lit

formNeg :: Formula -> Formula
formNeg = Neg

formAnd :: Formula -> Formula -> Formula
formAnd = And

formOr :: Formula -> Formula -> Formula
formOr = Or

formImplies :: Formula -> Formula -> Formula
formImplies = Implies

formIff :: Formula -> Formula -> Formula
formIff = Iff

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

negation :: Formula -> Formula

negation = recFormula
    (Neg . Lit)
    (\_ form -> form)
    (\rec1 rec2 _ _ -> Or rec1 rec2)
    (\rec1 rec2 _ _-> And rec1 rec2)
    (\_ rec2 form1 _ -> And form1 rec2)
    (\_ _ form1 form2 -> Or (And form1 (Neg form2)) (And (Neg form1) form2))