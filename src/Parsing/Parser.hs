module Parsing.Parser where

--   ( parseAll,
--   )

import Control.Monad
import Parsing.AST
import Text.ParserCombinators.Parsec


keywordsList :: [String]
keywordsList =
  [ "if",
    "elif",
    "else",
    "for",
    "while",
    "match",
    "break",
    "continue",
    "pass",
    "return",
    "class",
    "class_name",
    "extends",
    "is",
    "as",
    "self",
    "tool",
    "signal",
    "func",
    "static",
    "const",
    "enum",
    "var",
    "onready",
    "export",
    "setget",
    "breakpoint",
    "preload",
    "yield",
    "assert",
    "remote",
    "master",
    "puppet",
    "remotesync",
    "mastersync",
    "puppetsync",
    "PI",
    "TAU",
    "INF",
    "NAN"
  ]

builtInTypeList :: [String]
builtInTypeList =
  [ "null",
    "bool",
    "int",
    "float",
    "String",
    "Vector2",
    "Rect2",
    "Vector3",
    "Transform2D",
    "Plane",
    "Quat",
    "AABB",
    "Basis",
    "Transform",
    "Color",
    "NodePath",
    "RID",
    "Object",
    "Array",
    "Dictionary"
  ]

operatorList :: [String]
operatorList =
  [ "->",
    "[",
    "]",
    ".",
    "(",
    ")",
    "{",
    "}",
    "~",
    "-",
    "*",
    "/",
    "%",
    "+",
    "-",
    "<<",
    ">>",
    "&",
    "^",
    "|",
    "<",
    ">",
    "==",
    "!=",
    ">=",
    "<=",
    "in",
    "!",
    "not",
    "and",
    "&&",
    "or",
    "||",
    "=",
    "+=",
    "-=",
    "*=",
    "/=",
    "%=",
    "&=",
    "!=",
    "<<=",
    ">>=",
    ":=",
    ":"
  ]

pnth :: Parser ()
pnth = do
  return ()

fl :: Parser String
fl = do
  fail "exhausted reserved"

-- | Generates parsers from a list of strings.
genListParsers :: [String] -> Parser String
genListParsers = foldr ((<|>) . (try . parseWord)) fl

-- | Same function as genListParsers, except it takes a function as an extra argument
genListParsers' :: [String] -> (String -> Parser String) -> Parser String
genListParsers' x f = foldr ((<|>) . (try . f)) fl x

-- | Parses a single word.
parseWord :: String -> Parser String
parseWord str = do
  s <- string str
  notFollowedBy alphaNum
  return s

-- | Parses reserved keywords
keyword :: Parser AST
keyword = do
  _ <- many space
  pos <- getPosition
  let l = sourceLine pos
  let c = sourceColumn pos
  kw <- genListParsers keywordsList
  return $ Keyword kw (l, c)

-- | Parses built in types (and names them as keywords)
keyword' :: Parser AST
keyword' = do
  _ <- many space
  pos <- getPosition
  let l = sourceLine pos
  let c = sourceColumn pos
  kw' <- genListParsers builtInTypeList
  return $ Keyword kw' (l, c)

-- | Parses a comment
comment :: Parser AST
comment = do
  _ <- many space
  pos <- getPosition
  let l = sourceLine pos
  let c = sourceColumn pos
  char '#'
  m <- manyTill (anyChar <|> space') newline
  return $ Comment m (l, c)
  where
    space' = do
      char '\t' <|> char '\r' <|> char '\f' <|> char '\v' <|> char ' '

-- | Parses an operator
operator :: Parser AST
operator = do
  _ <- many space
  pos <- getPosition
  let l = sourceLine pos
  let c = sourceColumn pos
  op <- genListParsers' operatorList string
  return $ Operator op (l, c)

-- #TODO improve identifier later

-- | Parser an identifier
identifier :: Parser AST
identifier = do
  _ <- many space
  pos <- getPosition
  let l = sourceLine pos
  let c = sourceColumn pos
  id <- manyTill idParser (oneOf " []{}(),.:\n\t\r\f\v")
  return $ Identifier id (l, c)
  where
    idParser = alphaNum <|> char '_' <|> char '-' <|> char '"' <|> char '!'

chunk :: Parser AST
chunk =
    try keyword
    <|> try keyword'
    <|> try comment
    <|> try operator
    <|> identifier

parseAll :: Parser [AST]
parseAll = many chunk