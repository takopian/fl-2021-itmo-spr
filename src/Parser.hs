module Parser where

import Control.Applicative ((<|>))
import Data.Char (isDigit, digitToInt)
import Text.Printf (printf)

data Operator = Plus
              | Mult
              | Pow
              deriving (Show, Eq)

toOp :: Char -> Operator
toOp '+' = Plus
toOp '*' = Mult
toOp '^' = Pow
toOp c = error $ printf "Unsupported operator: %c" c

data Expr = BinOp Operator Expr Expr
          | Num Int
          deriving (Show, Eq)

eval :: Expr -> Int
eval (BinOp Plus l r) = eval l + eval r
eval (BinOp Mult l r) = eval l * eval r
eval (BinOp Pow e x) = eval e ^ eval x
eval (Num x) = x

data ParserType = Prefix | Infix deriving (Show)

parse :: ParserType -> String -> Maybe Expr
parse pType str =
    case go pType str of
      Just ("", e) -> Just e
      _ -> Nothing
  where
    go Prefix = parsePrefix
    go Infix  = parseInfix

-- Expr :: + Expr Expr
--       | * Expr Expr
--       | Digit
parsePrefix :: String -> Maybe (String, Expr)
parsePrefix (op : t) | op == '+' || op == '*' || op == '^' =
  case parsePrefix t of
    Just (t', l) ->
      case parsePrefix t' of
        Just (t'', r) -> Just (t'', BinOp (toOp op) l r)
        Nothing -> Nothing
    Nothing -> Nothing
parsePrefix (d : t) | isDigit d =
  Just (t, Num (digitToInt d))
parsePrefix _ = Nothing

-- Expr :: Expr + Expr
--       | Expr * Expr
--       | Digit
--       | ( Expr )

-- Expr :: Слаг + Слаг + ... + Слаг
--       = Слаг (+ Слаг) (+ Слаг) .. (+ Слаг)
--       -> [Слаг] - fold ... -> BinOp Plus (BinOp Plus ...)...
-- Слаг :: Множ * Множ * ... * Множ
-- Множ :: Expr ^ Expr ^ ... ^ Expr | Цифра | ( Expr )
-- [1,2,3] -> (1+2)+3

binOpLeft :: Operator -> [Expr] -> Expr
binOpLeft op = foldl1 (BinOp op)

binOpRight :: Operator -> [Expr] -> Expr
binOpRight op = foldr1 (BinOp op)

parseInfix :: String -> Maybe (String, Expr)
parseInfix = parseBase Plus

parseBase :: Operator -> String -> Maybe (String, Expr)
parseBase oper str =
    (binOp oper <$>) <$> go oper str
  where
    binOp = if oper == Pow then binOpRight else binOpLeft 
    go :: Operator -> String -> Maybe (String, [Expr])
    go oper str =
      let first = parseNext oper str in
      case first of
        Nothing -> Nothing
        Just (t, e) ->
          if null t
          then Just ("", [e])
          else
            case parseSign oper t of
              Just (t', _) ->
                let rest = go oper t' in
                ((e:) <$>) <$> rest
              Nothing -> Just (t, [e])

parseNext :: Operator -> String -> Maybe (String, Expr)
parseNext Plus str = parseBase Mult str
parseNext Mult str = parseBase Pow str
parseNext Pow str = parseDigit str <|> parseExprBr str

parseSign :: Operator -> String -> Maybe (String, Operator)
parseSign Plus = parsePlus
parseSign Mult = parseStar
parseSign Pow = parseHat

parseDigit :: String -> Maybe (String, Expr)
parseDigit (d : t) | isDigit d =
  Just (t, Num (digitToInt d))
parseDigit _ = Nothing

parseExprBr :: String -> Maybe (String, Expr)
parseExprBr ('(' : t) =
  case parseBase Plus t of
    Just ((')' : t'), e) -> Just (t', e)
    _ -> Nothing
parseExprBr _ = Nothing

parsePlus :: String -> Maybe (String, Operator)
parsePlus ('+' : t) = Just (t, Plus)
parsePlus _ = Nothing

parseStar :: String -> Maybe (String, Operator)
parseStar ('*' : t) = Just (t, Mult)
parseStar _ = Nothing

parseHat :: String -> Maybe (String, Operator)
parseHat ('^' : t) = Just (t, Pow)
parseHat _ = Nothing

plus :: Expr -> Expr -> Expr
plus = BinOp Plus

mult :: Expr -> Expr -> Expr
mult = BinOp Mult

pow :: Expr -> Expr -> Expr
pow = BinOp Pow
