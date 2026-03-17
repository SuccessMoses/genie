{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Void                  (Void)
import           Text.Megaparsec            ((<|>))
import qualified Text.Megaparsec            as Parsec
import qualified Text.Megaparsec.Char       as CharParser
import qualified Text.Megaparsec.Char.Lexer as Lexer
import           Control.Monad.Combinators.Expr
import qualified Control.Monad.Combinators.Expr as ExprParser


type Parser = Parsec.Parsec Void String

data Expr =
    Num Int
    | Add Expr Expr
    | Mul Expr Expr
    | Neg Expr
    deriving (Show)

space :: Parser ()
space = Lexer.space CharParser.space1 Parsec.empty Parsec.empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

symbol :: String -> Parser String
symbol = Lexer.symbol space

numLiteral :: Parser Expr
numLiteral = lexeme $ Num <$> Lexer.decimal

parens :: Parser a -> Parser a
parens = Parsec.between (symbol "(") (symbol ")")

termParser :: Parser Expr
termParser = parens expr <|> numLiteral

expr :: Parser Expr
expr = ExprParser.makeExprParser termParser termOperators

termOperators :: [[Operator Parser Expr]]
termOperators =
    [
        [ExprParser.Prefix (Neg <$ symbol "-")],
        [ExprParser.InfixL (Add <$ symbol "+")],
        [ExprParser.InfixL (Mul <$ symbol "*")]
    ]

-- so haskell does not complain
main :: IO ()
main = return ()
