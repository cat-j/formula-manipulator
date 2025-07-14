module Main (main) where

import Formula
import Test.HUnit

main :: IO ()

main = runTestTTAndExit allTests

allTests :: Test
allTests =
    test [
        "negation" ~: testsNegation,
        "negatedNormalForm" ~: testsNegatedNormalForm,
        "removeImplications" ~: testsRemoveImplications
    ]

testsNegation :: Test
testsNegation =
    test [
        negation (formLit "P") ~?= formNeg (formLit "P"),
        negation (formNeg (formLit "P")) ~?= formLit "P",
        negation (formAnd (formLit "P") (formLit "Q")) ~?= formOr (formNeg (formLit "P")) (formNeg (formLit "Q")),
        negation (formOr (formLit "P") (formLit "Q")) ~?= formAnd (formNeg (formLit "P")) (formNeg (formLit "Q")),
        negation (formImplies (formLit "P") (formLit "Q")) ~?= formAnd (formLit "P") (formNeg (formLit "Q")),
        negation (formIff (formLit "P") (formLit "Q")) ~?= formOr (formAnd (formLit "P") (formNeg (formLit "Q"))) (formAnd (formNeg (formLit "P")) (formLit "Q")),
        negation (formAnd (formNeg (formLit "P")) (formLit "Q")) ~?= formOr (formLit "P") (formNeg (formLit "Q")),
        negation (formImplies (formNeg (formLit "P")) (formLit "Q")) ~?= formAnd (formNeg (formLit "P")) (formNeg (formLit "Q")),
        negation (formOr (formAnd (formLit "P") (formLit "Q")) (formLit "R")) ~?= formAnd (formOr (formNeg (formLit "P")) (formNeg (formLit "Q"))) (formNeg (formLit "R")),
        negation (formImplies (formAnd (formLit "P") (formLit "Q")) (formOr (formLit "R") (formLit "S"))) ~?= formAnd (formAnd (formLit "P") (formLit "Q")) (formAnd (formNeg (formLit "R")) (formNeg (formLit "S"))),
        negation (formIff (formAnd (formLit "P") (formLit "Q")) (formOr (formLit "R") (formLit "S"))) ~?= formOr (formAnd (formAnd (formLit "P") (formLit "Q")) (formAnd (formNeg (formLit "R")) (formNeg (formLit "S")))) (formAnd (formOr (formNeg (formLit "P")) (formNeg (formLit "Q"))) (formOr (formLit "R") (formLit "S")))
    ]

testsNegatedNormalForm :: Test
testsNegatedNormalForm =
    test [
        negatedNormalForm (formLit "P") ~?= formLit "P",
        negatedNormalForm (formNeg (formLit "P")) ~?= formNeg (formLit "P"),
        negatedNormalForm (formNeg (formNeg (formLit "P"))) ~?= formLit "P",
        negatedNormalForm (formNeg (formAnd (formNeg (formLit "P")) (formLit "Q"))) ~?= formOr (formLit "P") (formNeg (formLit "Q")),
        negatedNormalForm (formNeg (formOr (formLit "P") (formNeg (formLit "Q")))) ~?= formAnd (formNeg (formLit "P")) (formLit "Q"),
        negatedNormalForm (formAnd (formNeg (formLit "P")) (formLit "Q")) ~?= formAnd (formNeg (formLit "P")) (formLit "Q"),
        negatedNormalForm (formAnd (formNeg (formNeg (formLit "P"))) (formLit "Q")) ~?= formAnd (formLit "P") (formLit "Q"),
        negatedNormalForm (formOr (formNeg (formLit "P")) (formLit "Q")) ~?= formOr (formNeg (formLit "P")) (formLit "Q")
    ]

testsRemoveImplications :: Test
testsRemoveImplications =
    test [
        removeImplications (formLit "P") ~?= formLit "P",
        removeImplications (formNeg (formLit "P")) ~?= formNeg (formLit "P"),
        removeImplications (formAnd (formLit "P") (formLit "Q")) ~?= formAnd (formLit "P") (formLit "Q"),
        removeImplications (formOr (formLit "P") (formLit "Q")) ~?= formOr (formLit "P") (formLit "Q"),
        removeImplications (formImplies (formLit "P") (formLit "Q")) ~?= formOr (formNeg (formLit "P")) (formLit "Q"),
        removeImplications (formIff (formLit "P") (formLit "Q")) ~?= formAnd (formOr (formNeg (formLit "P")) (formLit "Q")) (formOr (formLit "P") (formNeg (formLit "Q")))
    ]