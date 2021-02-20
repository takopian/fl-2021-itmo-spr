module Parser.Infix where

import Parser.Common (parseOp, parseDigit, parserEof)
import Control.Applicative ((<|>))
import Data.Char (isDigit, digitToInt, isSpace)
import Expr (Expr (..), Operator (..))
import Text.Printf (printf)
import Lexer (Token (..), lexer)

------------------------------------

parse :: String -> Maybe Expr
parse str = do
  tok <- lexer str  
  res <- parserEof parseSum tok
  return res

-- Expr :: Expr + Expr
--       | Expr * Expr
--       | Expr ^ Expr
--       | Digit
--       | ( Expr )

-- Expr :: Слаг + Слаг + ... + Слаг
--       = Слаг (+ Слаг) (+ Слаг) .. (+ Слаг)
--       -> [Слаг] - fold ... -> BinOp Plus (BinOp Plus ...)...
-- Слаг :: Множ * Множ * ... * Множ
-- Множ :: Фактор ^ Фактор ^ ... ^ Фактор
-- Фактор :: Цифра | ( Expr )
-- [1,2,3] -> (1+2)+3

data Associativity = AssocL | AssocR

binOp :: Associativity -> Operator -> [Expr] -> Expr
binOp AssocL op = foldl1 (BinOp op)
binOp AssocR op = foldr1 (BinOp op)

parseBinOp :: Associativity -> Operator -> ([Token] -> Maybe ([Token], Expr)) -> [Token] -> Maybe ([Token], Expr)
parseBinOp assoc op nextParser tok =
    (binOp assoc op <$>) <$> go tok
  where
    go :: [Token] -> Maybe ([Token], [Expr])
    go tok = do
      first@(t, e) <- nextParser tok
      if null t
      then return (t, [e])
      else
        ( do
          (t', _) <- checkOper op t
          let rest = go t'
          ((e:) <$>) <$> rest
        )
        <|>
        return (t, [e])

checkOper :: Operator -> [Token] -> Maybe ([Token], Operator)
checkOper oper (Oper o : t) | oper == o = Just (t, oper)
checkOper _ _ = Nothing

checkDigit :: [Token] -> Maybe ([Token], Expr)
checkDigit (Number n : t) = Just (t, Num n)
checkDigit _ = Nothing

parseSum :: [Token] -> Maybe ([Token], Expr)
parseSum = parseBinOp AssocL Plus parseMult

parseMult :: [Token] -> Maybe ([Token], Expr)
parseMult = parseBinOp AssocL Mult parsePow

parsePow :: [Token] -> Maybe ([Token], Expr)
parsePow = parseBinOp AssocR Pow (\str -> checkDigit str <|> parseExprBr str)

parseExprBr :: [Token] -> Maybe ([Token], Expr)
parseExprBr (Lbr : t) = 
  case parseSum t of
    Just (Rbr : t', e) -> Just (t', e)
    _ -> Nothing
parseExprBr _ = Nothing