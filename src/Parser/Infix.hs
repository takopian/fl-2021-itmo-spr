{-# LANGUAGE LambdaCase #-}
module Parser.Infix where

import Expr ( Expr (..), Operator, toOp )
import Parser.Common ( number, alpha, digit, uberExpr, Associativity (..) )
import Parser.Combinators ( Parser, eof, whitespaces, parseEither, brackets, word, symbol )
import Control.Applicative (some, many, (<|>))


multP = toOp <$> word "*"
sumP = toOp <$> word "+"
minusP = toOp <$> word "-"
divP = toOp <$> word "/"
powP = toOp <$> word "^"
eqP = toOp <$> word "=="
neqP = toOp <$> word "/="
geP = toOp <$> word ">="
gtP = toOp <$> word ">"
leP = toOp <$> word "<="
ltP = toOp <$> word "<"

noAssocOps :: (Parser String Operator, Associativity)
noAssocOps = (eqP <|> neqP <|> geP <|> gtP <|> leP <|> ltP, NoAssoc)

sumSubOps :: (Parser String Operator, Associativity)
sumSubOps = (sumP <|> minusP, LeftAssoc)

multDivOps :: (Parser String Operator, Associativity)
multDivOps = (multP <|> divP, LeftAssoc)

powOp :: (Parser String Operator, Associativity)
powOp = (powP, RightAssoc)

ident = do
  first <- alpha <|> symbol '_'
  rest <- many (alpha <|> digit <|> symbol '_')
  return (first : rest)


parse :: String -> Either String Expr
parse =
  parseEither (whitespaces *> parseExpr <* whitespaces <* eof)

parseExpr :: Parser String Expr
parseExpr = 
  uberExpr [ noAssocOps
           , sumSubOps
           , multDivOps
           , powOp
           ]
           ( whitespaces *> (
             Num <$> number
             <|> Ident <$> ident
             <|> symbol '(' *> parseExpr <* symbol ')'
             ) <* whitespaces
           )
           BinOp

