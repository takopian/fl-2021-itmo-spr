module Test.Common where

import Data.Either (isRight, isLeft, fromRight)
import Test.Tasty.HUnit ((@?=), Assertion, assertBool)

syntaxError :: (String -> Either a b) -> String -> Assertion
syntaxError parser input = assertBool "Expected syntax error" (isLeft $ parser input)

parsingSuccess :: (Show b, Eq b) => (String -> Either a b) -> String -> b -> Assertion
parsingSuccess parser input exp = do
  let result = parser input
  assertBool "Expected parsing to succeed" (isRight result)
  fromRight undefined result @?= exp 