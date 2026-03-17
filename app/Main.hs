{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Void                  (Void)
import           Text.Megaparsec            ((<|>))
import qualified Text.Megaparsec            as Parsec
import qualified Text.Megaparsec.Char       as CharParser
import qualified Text.Megaparsec.Char.Lexer as Lexer
import           Control.Monad.Combinators.Expr

{-
  WHILE Language Grammar
  a   ::= x | n | - a | a opa a
  b   ::= true | false | not b | b opb b | a opr a
  opa ::= + | - | * | /
  opb ::= and | or
  opr ::= > | <
  S   ::= x := a | skip | S1; S2 | ( S ) | if b then S1 else S2 | while b do S
-}

data ArithExpr = 
    Variable String
    | Num Int
    | Neg ArithExpr
    | Opa Opa ArithExpr ArithExpr

data BoolExpr = 
    True
    | False
    | Not BoolExpr
    | Opb Opb BoolExpr BoolExpr
    | Opr Opr ArithExpr ArithExpr

data Opa = Add | Sub | Mul | Div

data Opb = And | Or

data Opr = LT | GT

data Stmt =
    Assign String ArithExpr
    | Skip
    | Seq [Stmt]
    | If BoolExpr Stmt Stmt
    | While BoolExpr Stmt


type Parser = Parsec.Parsec Void String

space :: Parser ()
space = Lexer.space CharParser.space1 lineComment blockComment
  where
    lineComment = Lexer.skipLineComment "//"
    blockComment = Lexer.skipBlockComment "/*" "*/"

symbol :: String -> Parser String
symbol = Lexer.symbol space

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

reserved :: String -> Parser ()
reserved w = 
    lexeme $ Parsec.try $ 
        CharParser.string w *> Parsec.notFollowedBy CharParser.alphaNumChar

reservedWords :: [String]
reservedWords = 
    [
        "if",
        "then",
        "else",
        "while",
        "do",
        "skip",
        "true",
        "false",
        "not",
        "and",
        "or"
    ]

identifier :: Parser String
identifier = lexeme $ Parsec.try $ do
    x <- (:) <$> CharParser.letterChar <*> Parsec.many CharParser.alphaNumChar
    if x `elem` reservedWords
    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
    else return x


-- so haskell does not complain
main :: IO ()
main = return ()
