module Formula
    (Formula, formLit, formNeg, formAnd, formOr, formImplies, formIff,
    negation, negatedNormalForm, removeImplications)
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
    (\rec1 rec2 form1 form2 -> Or (And form1 rec2) (And rec1 form2))


negatedNormalForm :: Formula -> Formula

negatedNormalForm (Lit s) = Lit s

negatedNormalForm (Neg aFormula) =
    case aFormula of
        Lit _ -> Neg aFormula
        Neg subFormula -> subFormula
        And subFormula1 subFormula2 -> Or (negatedNormalForm (Neg subFormula1)) (negatedNormalForm (Neg subFormula2))
        Or subFormula1 subFormula2 -> And (negatedNormalForm (Neg subFormula1)) (negatedNormalForm (Neg subFormula2))
        _somethingWithImplications -> error "Formula must contain neither => nor <=>."

negatedNormalForm (And subFormula1 subFormula2) =
    And (negatedNormalForm subFormula1) (negatedNormalForm subFormula2)

negatedNormalForm (Or subFormula1 subFormula2) =
    Or (negatedNormalForm subFormula1) (negatedNormalForm subFormula2)

negatedNormalForm _ = error "Formula must contain neither => nor <=>."


removeImplications :: Formula -> Formula

removeImplications = foldFormula
    Lit
    Neg
    And
    Or
    (Or . Neg)
    (\rec1 rec2 -> And (Or (Neg rec1) rec2) (Or rec1 (Neg rec2)))