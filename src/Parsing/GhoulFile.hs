module Parsing.GhoulFile (parseRules) where

import Control.Monad
import PrettyPrint.Styles (OutputStyle (OutputStyle), defaultTheme1)
import Rules.Rules (Rule (..), RuleQuestionnaire (..))
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

parseRules :: String -> FilePath -> Either ParseError [RuleQuestionnaire]
parseRules s f = parse r f s

parseStyle :: String -> FilePath -> Either ParseError OutputStyle
parseStyle s f = parse os f s

test = parseStyle txt "test file"

txt = "[Rules]\n    StaticTyping = True\n    ProperComments = True\n[Style]\nFileColor = (255,255,255)\n    FileColorPath = Black\n    LineColor = Blue\n    LineNumberColor = Blue\n    CodeColor = Blue\n    RuleColor = Blue\n    SeparatorColor = Red"

r = parseSignature "[Rules]" >> many (try parseRule)

os = do
  manyTill anyToken (try $ string "[Style]")
  many space
  -- string "[Style]"
  -- parseSignature "[Style]"
  parseStyles

-- val :: Either a b -> a
val (Left x) = x
-- val (Right x) = x

parseSignature :: String -> Parser String
parseSignature sig = do
  _ <- many space
  s <- string sig
  notFollowedBy alphaNum
  return s

parseRule :: Parser RuleQuestionnaire
parseRule = do
  _ <- many space
  s <- many alphaNum

  _ <- many space
  _ <- char '='
  _ <- many space
  bool <- parseFalse <|> parseTrue
  return $ RuleQ (fromStringToRule s) bool

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

  return $ OutputStyle (read fileColor) (read fileColorPath) (read lineColor) (read lineNumberColor) (read codeColor) (read ruleColor) (Just $ read separator)

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

fromStringToRule :: String -> Rule
fromStringToRule "StaticTyping" = StaticTypes
fromStringToRule "ProperComments" = ProperComments
fromStringToRule "ProperOrdering" = ProperOrdering
fromStringToRule "DeepNode" = DeepNode
