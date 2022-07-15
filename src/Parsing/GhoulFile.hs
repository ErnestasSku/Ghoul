module Parsing.GhoulFile (parseRules, parseStyle) where

import Control.Monad
import PrettyPrint.Styles (OutputStyle (OutputStyle), defaultTheme1)
import Rules.Rules
import Text.ParserCombinators.Parsec
  ( ParseError,
    Parser,
    alphaNum,
    anyChar,
    anyToken,
    char,
    many,
    manyTill,
    notFollowedBy,
    parse,
    space,
    string,
    try,
    (<|>), digit
  )
import PrettyPrint.Pretty (Color)
import Data.Foldable (Foldable(foldr', fold))
import Text.Parsec.Token (GenTokenParser(integer))
import Parsing.Internal.ParserInternal (parseWord)

-- ==================== Ghoul File Parsing Section ====================  

parseRules :: FilePath -> String -> Either ParseError [CompoundRule]
parseRules = parse r

parseStyle :: FilePath -> String  -> Either ParseError OutputStyle
parseStyle = parse os

r = parseSignature "[Rules]" >> many (try parseRule)

os = do
  manyTill anyToken (try $ string "[Style]")
  many space
  parseStyles

parseSignature :: String -> Parser String
parseSignature sig = do
  _ <- many space
  s <- string sig
  notFollowedBy alphaNum
  return s

parseRule :: Parser CompoundRule
parseRule = do
  _ <- many space
  s <- genRuleParser (map show $ (fullRuleList :: [CompoundRule]))
  return (read s :: CompoundRule)

genRuleParser :: [String] -> Parser String
genRuleParser = foldr ((<|>) . (try . parseWord)) fl

fl :: Parser String
fl = do
  fail "exhausted reserved"


parseTrue :: Parser Bool
parseTrue = do
  _ <- string "True"
  notFollowedBy alphaNum
  return True

parseFalse :: Parser Bool
parseFalse = do
  _ <- string "False"
  notFollowedBy alphaNum
  return False

parseStyles :: Parser OutputStyle
parseStyles = do
  many space
  string "FileColor = "
  fileColor <- try parseColor <|> parseColor'
  many space
  string "FileColorPath = "
  fileColorPath <- try parseColor <|> parseColor'
  many space
  string "LineColor = "
  lineColor <- try parseColor <|> parseColor'
  many space
  string "LineNumberColor = "
  lineNumberColor <- try parseColor <|> parseColor'
  many space
  string "CodeColor = "
  codeColor <- try parseColor <|> parseColor'
  many space
  string "RuleColor = "
  ruleColor <- try parseColor <|> parseColor'
  many space
  string "SeparatorColor = "
  separator <- try parseColor <|> try parseColor' <|> specialSepField

  return $ OutputStyle (read fileColor) (read fileColorPath) (read lineColor) (read lineNumberColor) (read codeColor) (read ruleColor) (read' separator)
  where
    read' "None" = Nothing
    read' x = Just $ read x

colors :: [String]
colors =
  [ "Black",
    "Red",
    "Green",
    "Yellow",
    "Blue",
    "Magenta",
    "Cyan",
    "White",
    "Default"
  ]

parseColor :: Parser String
parseColor = foldr ((<|>) . try . string) fl colors
  where
    fl = fail "exhausted"

parseColor' :: Parser String
parseColor' = do
  char '('
  r <- many digit
  char ','
  g <- many digit
  char ','
  b <- many digit
  char ')'

  return $ "Custom " <> "(" <> r <> "," <> g <> "," <> b <> ")"

specialSepField :: Parser String
specialSepField = string "None"

