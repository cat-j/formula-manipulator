module Main (main) where

import Formula
import Test.HUnit

main :: IO ()

main = runTestTTAndExit allTests

allTests :: Test
allTests =
    test [
        "negate" ~: testsNegation
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
        negation (formOr (formAnd (formLit "P") (formLit "Q")) (formLit "R")) ~?= formAnd (formOr (formNeg (formLit "P")) (formNeg (formLit "Q"))) (formNeg (formLit "R"))
    ]