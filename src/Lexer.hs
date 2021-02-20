module Lexer where

import Control.Applicative ((<|>))
import Data.Char (isDigit, digitToInt, isSpace)
import Text.Printf (printf)
import Expr (Operator (..), toOp)

-- "+77*    123 42" -lexer-> [Plus, Num 77, Mult, Num 123, Num 42]
-- "+77*    123 42!" -lexer-> Oops
data Token = Oper Operator | Number Int | Lbr | Rbr
           deriving (Show, Eq)

toBracket :: Char -> Token
toBracket '(' = Lbr
toBracket ')' = Rbr


lexer :: String -> Maybe [Token]
lexer [] = Just []
lexer (h:t) | h `elem` "+*^" = do
  rest <- lexer t
  return (Oper (toOp h) : rest)
lexer (h:t) | h `elem` "()" = do
    rest <- lexer t
    return (toBracket h : rest)
lexer (h:t) | isDigit h = do
    (t', number) <- accumulateNumber [h] t
    rest <- lexer t'
    return (Number (read number :: Int) : rest)
  where
    accumulateNumber acc (d:t) | isDigit d = accumulateNumber (d:acc) t
    accumulateNumber acc t = return (t, reverse acc)
lexer (h:t) | isSpace h = lexer t
lexer _ = Nothing