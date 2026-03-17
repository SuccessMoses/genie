{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Void                  (Void)
import           Text.Megaparsec            ((<|>))
import qualified Text.Megaparsec            as Parsec
import qualified Text.Megaparsec.Char       as CharParser
import qualified Text.Megaparsec.Char.Lexer as Lexer
import           Control.Monad.Combinators.Expr
import qualified Control.Monad.Combinators.Expr as ExprParser

{-
  ARITH Language Grammar
  a ::= 0 | a++ | a opa a | if b then a else a
  b ::= true | false | not b | b opb b | a opr a
  t   ::=  a | b
  opa ::= + | - | *
  opb ::= and | or
  opr ::= > | <
  S   ::= x := t | skip
  Seq ::= null | S; Seq
-}

type Parser = Parsec.Parsec Void String

data AExpr =
    ZeroE
    | SuccE AExpr
    | OpEA OpA AExpr AExpr
    | IfE BExpr AExpr AExpr
    deriving (Show)

data BExpr =
    TrueE
    | FalseE
    | Not BExpr
    | OpEB OpB BExpr BExpr
    | OpER OpR AExpr AExpr
    deriving (Show)

data OpA = AddE | SubE | MulE deriving (Show)

data OpR = LTE | GTE deriving (Show)

data OpB = AndE | OrE deriving (Show)

space :: Parser ()
space = Lexer.space CharParser.space1 Parsec.empty Parsec.empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

symbol :: String -> Parser String
symbol = Lexer.symbol space

parens :: Parser a -> Parser a
parens = Parsec.between (symbol "(") (symbol ")")

aLit :: Parser AExpr
aLit = lexeme $ do
    n <- (Lexer.decimal :: Parser Int)
    return $ foldr (\_ acc -> SuccE acc) ZeroE [1..n]

ifParser :: Parser AExpr
ifParser = Parsec.try $ do
    _ <- symbol "if"
    b <- bExpr
    _ <- symbol "then"
    a1 <- aExpr
    _ <- symbol "else"
    IfE b a1 <$> aExpr

aExprTerm :: Parser AExpr
aExprTerm = 
    aLit
    <|> ifParser
    <|> parens aExpr

aExpr :: Parser AExpr
aExpr = ExprParser.makeExprParser aExprTerm [[]]

bExpr :: Parser BExpr
bExpr = ExprParser.makeExprParser bExprTerm [[]]
    where
        bExprTerm = 
            TrueE <$ symbol "true"
            <|> FalseE <$ symbol "false"
            <|> parens bExpr

term :: Parser (Either AExpr BExpr)
term = (Left <$> aExpr) <|> (Right <$> bExpr)

-- expr :: Parser Expr
-- expr = ExprParser.makeExprParser termParser termOperators

-- termOperators :: [[Operator Parser Expr]]
-- termOperators =
--     [
--         [ExprParser.Prefix (Neg <$ symbol "-")],
--         [ExprParser.InfixL (Add <$ symbol "+")],
--         [ExprParser.InfixL (Mul <$ symbol "*")]
--     ]

-- so haskell does not complain
main :: IO ()
main = return ()
