module Parser where

import SyntaxParser.SyntaxParser
import SyntaxParser.AstStmt ( Program )

data ParserType = Mega deriving (Show)

parse :: ParserType -> String -> Either String Program
parse Mega = SyntaxParser.SyntaxParser.parser

