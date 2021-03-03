module Parser.Common where

import Data.Char (isDigit, isAlpha)
import Parser.Combinators ( Parser, satisfy)
import Control.Applicative (some, many, (<|>))

digit :: Parser String Char
digit = satisfy isDigit

digit' :: Parser String Char
digit' = satisfy (\x -> isDigit x && x /= '0')

alpha :: Parser String Char
alpha = satisfy isAlpha

-- number :: not empty, consists of digits
number :: Parser String Int
number = do
  num <- some digit
  return $ (read num :: Int)

data Associativity = LeftAssoc  -- x `op` y `op` z == (x `op` y) `op` z
                   | RightAssoc -- x `op` y `op` z == x `op` (y `op` z)
                   | NoAssoc    -- non associative operation:
                                -- x `op` y -- ok
                                -- x `op` y `op` z -- not ok


sepByOper :: Parser input op -> Parser input elem -> Parser input ([op], [elem])
sepByOper sep elem = do
  h <- elem
  l_of_pairs <- many ( (,) <$> sep <*> elem)
  return (map fst l_of_pairs, h : map snd l_of_pairs)


uberExpr :: [(Parser i op, Associativity)]
         -> Parser i ast
         -> (op -> ast -> ast -> ast)
         -> Parser i ast

uberExpr ((op, NoAssoc):ops) parser build = 
  ( do
    left <- uberExpr ops parser build 
    o <- op
    right <- uberExpr ops parser build 
    return $ build o left right
  ) <|>
  uberExpr ops parser build 

uberExpr ((op, LeftAssoc):ops) parser build = do
  (ops, elems) <- sepByOper op (uberExpr ops parser build)
  let rest = zipWith flip (map build ops) (tail elems)
  return $ foldl (\ f r -> r f) (head elems) rest 

uberExpr ((op, RightAssoc):ops) parser build = do
  (ops, elems) <- sepByOper op (uberExpr ops parser build)
  let rest = zipWith ($) (map build ops) (init elems)
  return $ foldr ($)  (last elems) rest

uberExpr [] p _ = p