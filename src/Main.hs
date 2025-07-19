module Main (main) where

import Formula
import Test.HUnit

main :: IO ()

main = runTestTTAndExit allTests

allTests :: Test
allTests =
    test [
        -- "negation" ~: testsNegation,
        -- "negatedNormalForm" ~: testsNegatedNormalForm,
        "removeImplications" ~: testsRemoveImplications
    ]

-- testsNegation :: Test
-- testsNegation =
--     test [
--         negation (Lit "P") ~?= Neg (Lit "P"),
--         negation (Neg (Lit "P")) ~?= Lit "P",
--         negation (And (Lit "P") (Lit "Q")) ~?= Or (Neg (Lit "P")) (Neg (Lit "Q")),
--         negation (Or (Lit "P") (Lit "Q")) ~?= And (Neg (Lit "P")) (Neg (Lit "Q")),
--         negation (Implies (Lit "P") (Lit "Q")) ~?= And (Lit "P") (Neg (Lit "Q")),
--         negation (Iff (Lit "P") (Lit "Q")) ~?= Or (And (Lit "P") (Neg (Lit "Q"))) (And (Neg (Lit "P")) (Lit "Q")),
--         negation (And (Neg (Lit "P")) (Lit "Q")) ~?= Or (Lit "P") (Neg (Lit "Q")),
--         negation (Implies (Neg (Lit "P")) (Lit "Q")) ~?= And (Neg (Lit "P")) (Neg (Lit "Q")),
--         negation (Or (And (Lit "P") (Lit "Q")) (Lit "R")) ~?= And (Or (Neg (Lit "P")) (Neg (Lit "Q"))) (Neg (Lit "R")),
--         negation (Implies (And (Lit "P") (Lit "Q")) (Or (Lit "R") (Lit "S"))) ~?= And (And (Lit "P") (Lit "Q")) (And (Neg (Lit "R")) (Neg (Lit "S"))),
--         negation (Iff (And (Lit "P") (Lit "Q")) (Or (Lit "R") (Lit "S"))) ~?= Or (And (And (Lit "P") (Lit "Q")) (And (Neg (Lit "R")) (Neg (Lit "S")))) (And (Or (Neg (Lit "P")) (Neg (Lit "Q"))) (Or (Lit "R") (Lit "S")))
--     ]

-- testsNegatedNormalForm :: Test
-- testsNegatedNormalForm =
--     test [
--         negatedNormalForm (Lit "P") ~?= Lit "P",
--         negatedNormalForm (Neg (Lit "P")) ~?= Neg (Lit "P"),
--         negatedNormalForm (Neg (Neg (Lit "P"))) ~?= Lit "P",
--         negatedNormalForm (Neg (And (Neg (Lit "P")) (Lit "Q"))) ~?= Or (Lit "P") (Neg (Lit "Q")),
--         negatedNormalForm (Neg (Or (Lit "P") (Neg (Lit "Q")))) ~?= And (Neg (Lit "P")) (Lit "Q"),
--         negatedNormalForm (And (Neg (Lit "P")) (Lit "Q")) ~?= And (Neg (Lit "P")) (Lit "Q"),
--         negatedNormalForm (And (Neg (Neg (Lit "P"))) (Lit "Q")) ~?= And (Lit "P") (Lit "Q"),
--         negatedNormalForm (Or (Neg (Lit "P")) (Lit "Q")) ~?= Or (Neg (Lit "P")) (Lit "Q"),

--         -- ¬P => Q = P || Q
--         negatedNormalForm (Implies (Neg (Lit "P")) (Lit "Q")) ~?= Or (Lit "P") (Lit "Q"),

--         -- P <=> ¬Q = (¬P || ¬Q) && (P || Q)
--         negatedNormalForm (Iff (Lit "P") (Neg (Lit "Q"))) ~?= And (Or (Neg (Lit "P")) (Neg (Lit "Q"))) (Or (Lit "P") (Lit "Q"))
--     ]

testsRemoveImplications :: Test
testsRemoveImplications =
    test [
        removeImplications (Lit "P") ~?= Lit "P",
        removeImplications (Neg (Lit "P")) ~?= Neg (Lit "P"),
        removeImplications (And (Lit "P") (Lit "Q")) ~?= And (Lit "P") (Lit "Q"),
        removeImplications (Or (Lit "P") (Lit "Q")) ~?= Or (Lit "P") (Lit "Q"),
        removeImplications (Implies (Lit "P") (Lit "Q")) ~?= Or (Neg (Lit "P")) (Lit "Q"),
        removeImplications (Iff (Lit "P") (Lit "Q")) ~?= And (Or (Neg (Lit "P")) (Lit "Q")) (Or (Lit "P") (Neg (Lit "Q")))
    ]