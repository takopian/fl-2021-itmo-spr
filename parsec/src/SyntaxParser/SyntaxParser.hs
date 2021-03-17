module SyntaxParser.SyntaxParser where

import Text.Megaparsec
    ( runParser,
      errorBundlePretty,
      many,
      Parsec,
      ShowErrorComponent,
      TraversableStream,
      VisualStream,
      (<|>),
      empty,
      MonadParsec(try),
      some )
import Data.Void (Void(..)) 
import Text.Megaparsec.Char
    ( alphaNumChar, char, letterChar, space1 )
import Control.Monad.Combinators.Expr
    ( makeExprParser, Operator(InfixN, InfixL, InfixR) )
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as Map
import qualified SyntaxParser.AstStmt as Ast

type Parser = Parsec Void String

kw :: [String]
kw = ["if", "then", "else", "while", "input", "output", "true", "false"]

generalBinOpParser :: (Parser (Ast.Expr -> Ast.Expr -> Ast.Expr) -> t) -> String -> t
generalBinOpParser parserType name = parserType (Ast.BinOp . (Ast.stringToOperator Map.!) <$> word name)

lBinOpParser :: String -> Operator Parser Ast.Expr
lBinOpParser = generalBinOpParser InfixL
rBinOpParser :: String -> Operator Parser Ast.Expr
rBinOpParser = generalBinOpParser InfixR
nBinOpParser :: String -> Operator Parser Ast.Expr
nBinOpParser = generalBinOpParser InfixN

operators :: [[Operator Parser Ast.Expr]]
operators = [
    [rBinOpParser "^" ],
    [lBinOpParser "*", lBinOpParser "/"],
    [lBinOpParser "+", lBinOpParser "-"],
    [nBinOpParser "==", nBinOpParser "!=", 
    nBinOpParser "<=", nBinOpParser ">=", 
    nBinOpParser "<", nBinOpParser ">" ]]

spaces :: Parser ()
spaces =  L.space space1 empty empty

lexemeSplitter :: Parser a -> Parser a
lexemeSplitter = L.lexeme spaces

word :: String -> Parser String
word = L.symbol spaces

numParser :: Parser Ast.Expr
numParser = lexemeSplitter (parser <|> parserNegate) where 
    parser = Ast.Num <$> L.decimal 
    parserNegate = do 
        word "("
        value <- L.signed spaces L.decimal
        word ")"
        return $ Ast.Num value

identParser :: Parser Ast.Expr 
identParser = Ast.Ident <$> (lexemeSplitter . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many (char '_' <|> alphaNumChar)
    check x = if x `elem` kw
              then fail $ show x ++ " cannot be an identifier"
              else return x

identNumberParser :: Parser Ast.Expr
identNumberParser = lexemeSplitter $ identParser <|> numParser

binOpParser :: Parser Ast.Expr
binOpParser = makeExprParser ((word "(" *> binOpParser <* word ")") <|> identNumberParser) operators

exprParser :: Parser Ast.Expr 
exprParser = binOpParser <|> identNumberParser

conditionParser :: Parser Ast.Expr   
conditionParser = lexemeSplitter (word "(" *> exprParser <* word ")")

ignoreParser :: Parser Ast.Stmt 
ignoreParser = lexemeSplitter $ do
    expr <- exprParser
    end <- word ";"
    return $ Ast.Ignore expr

assignParser :: Parser Ast.Stmt 
assignParser = lexemeSplitter $ do
    (Ast.Ident name) <- identParser
    word "[=]"
    value <- exprParser
    word ";"
    return $ Ast.Assign name value

writeParser :: Parser Ast.Stmt 
writeParser = lexemeSplitter $ do 
    word "write"
    value <- exprParser
    word ";"
    return $ Ast.Write value

readParser :: Parser Ast.Stmt 
readParser = lexemeSplitter $ do 
    word "read"
    (Ast.Ident value) <- identParser
    word ";"
    return $ Ast.Read value

ifParser :: Parser Ast.Stmt 
ifParser = lexemeSplitter $ do 
    word "if"
    condition <- conditionParser
    word "then"
    ifStatement <- compoundStatementParser
    Ast.If condition ifStatement . Just <$> (word "else" *> compoundStatementParser)
    

whileParser :: Parser Ast.Stmt 
whileParser = lexemeSplitter $ do
    word "while"
    condition <- conditionParser
    Ast.While condition <$> compoundStatementParser

statementParser :: Parser Ast.Stmt 
statementParser = lexemeSplitter $ ifParser <|> whileParser <|> assignParser <|> readParser <|> writeParser <|> ignoreParser

sequenceParser :: Parser Ast.Stmt 
sequenceParser = lexemeSplitter $ Ast.Seq <$> some statementParser

compoundStatementParser :: Parser Ast.Stmt 
compoundStatementParser = lexemeSplitter $ do 
    word "{"
    statements <- many sequenceParser
    word "}"
    return $ Ast.Seq statements

progParser :: Parser Ast.Program
progParser = spaces *> (Ast.Program <$> sequenceParser)  <* spaces 

parser :: String -> Either String Ast.Program
parser = runBundlingParser progParser 

runBundlingParser :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Parsec e s b -> s -> Either String b
runBundlingParser parser =
    mapLeft errorBundlePretty . runParser parser ""
  where
    mapLeft f = either (Left . f) Right