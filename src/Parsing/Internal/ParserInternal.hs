module Parsing.Internal.ParserInternal
  ( parseAll,
    chunk,
    identifier,
    operator,
    comment,
    keyword',
    keyword,
    parseWord
  )
where

import Control.Monad
import Data.Char (isAlphaNum)
import Data.Monoid (Any (Any, getAny))
import Parsing.AST
import Parsing.Internal.GdKeywords
import Text.Parsec (modifyState)
import Text.ParserCombinators.Parsec

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

-- | Parser an identifier
identifier :: Parser AST
identifier = do
  _ <- many space
  _ <- many $ char ','
  _ <- many space
  pos <- getPosition
  let l = sourceLine pos
  let c = sourceColumn pos

  f <- idParser
  id <- many idParser
  return $ Identifier (f : id) (l, c)
  where
    idParser = satisfy acceptable
      where
        {-
            Explanation of how this function works.

            If something is a monoid, you can use a mappend function which looks like a -> a -> a
            It's a binary operation which combines 2 monoids into one single monoid.
            (->) operator is also a monoid, :info (->) gives us
            instance Monoid b => Monoid (a -> b), therefore, our second operator has to be a monoid.

            We are working with Char -> Bool function, but Bool is not a monoid (as there is too many ways to define a monoid for booleans)
            So we use Any which is a monoid.

            foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

        -}
        acceptable :: Char -> Bool
        acceptable = getAny . foldMap (Any .) [isAlphaNum, (== '_'), (== '"'), (== '$'), (==';')]

chunk :: Parser AST
chunk =
  try keyword
    <|> try keyword'
    <|> try comment
    <|> try operator
    <|> try identifier

parseAll :: Parser [AST]
parseAll = many chunk