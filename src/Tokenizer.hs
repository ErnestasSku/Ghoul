{-
    Initial version of Tokenizer. 
-}

{-# LANGUAGE LambdaCase #-}
module Tokenizer where

import Control.Applicative ( Alternative((<|>), empty), optional, many )
import Data.Char
import Data.Maybe

-- FUTURE: Everything is a string now. 
-- In the future it might be benefocial (or not) to make a n ew data type for Operators or Seperators 
data Token
    = Identifier String
    | Keyword String
    | Seperator String
    | Operator String
    -- | Literal String -- NOTE: Decided on not using Literal type, as I wouldn't have an easy way to tell Identifier and Literall appart 
    | Comment String
    | Error --  

    deriving (Show, Eq)

-- NOTE: it does not have error reporting. 
-- In the happy path, it should never really produce any errors,
-- since this tool is supposed to be used along side Godot's usual code checkers
-- and this used only for enforcing certain style
-- TL;DR This program should only be fed valid code
-- Note: This parse may be slow, as It's using String at the moment. Consider using Text in the future.
newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input -> do
            (input', x) <- p input
            Just (input', f x)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) =
        Parser $ \input -> do
            (input', f) <- p1 input
            (input'', a) <- p2 input'
            Just (input'', f a)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) =
        Parser $ \input -> p1 input <|> p2 input


instance Monad Parser where
    (Parser p) >>= f = Parser $ \input -> case p input of
        Nothing         -> Nothing
        Just (rest, o)  -> runParser (f o) rest

parseChar :: Char -> Parser Char
parseChar x = Parser f
    where
        f (y:ys)
          | y == x = Just (ys, x)
          | otherwise = Nothing
        f [] = Nothing

parseSequence :: String -> Parser String
parseSequence = traverse parseChar

parsePredicate :: (Char -> Bool) -> Parser Char
parsePredicate f = Parser $ \case
    (x:xs) | f x -> Just (xs, x)
    _            -> Nothing

parseWhile :: (Char -> Bool) -> Parser String
parseWhile f = Parser $ \input ->
    let (token, rest) = span f input
    in if null token then Nothing
        else Just (rest, token)

parseWord :: String -> Parser String
parseWord s =
    do
        parsed <- parseSequence s
        -- Check if after parsing specific word, there is still leters 
        leftover <- optional (parsePredicate isAlpha)

        -- If there are letters, that means it's not a full word, therefore we need to not consume the input (Fail parser)
        if isJust leftover
            then parserFail     -- Probably not the smartest (Haskelly way) of doing things. Haskellify later
            else return parsed

    where
        -- FIXME: this is a very dumb looking solution, learn more, and think of a better solution
        parserFail :: Parser String
        parserFail = Parser $ const Nothing

parseSeperator :: String -> Parser String
parseSeperator s =
    do
        parseSequence s

keywordsList :: [String]
keywordsList = ["if", "elif", "else", "for", "while", "match",
                "break", "continue", "pass", "return", "class",
                "class_name", "extends", "is", "as", "self", "tool",
                "signal", "func", "static", "const", "enum", "var",
                "onready", "export", "setget", "breakpoint", "preload",
                "yield", "assert", "remote", "master", "puppet", "remotesync",
                "mastersync", "puppetsync", "PI", "TAU", "INF", "NAN"]

builtInTypeList :: [String]
builtInTypeList =  ["null", "bool", "int", "float", "String",
                    "Vector2", "Rect2", "Vector3", "Transform2D",
                    "Plane", "Quat", "AABB", "Basis", "Transform",
                    "Color", "NodePath", "RID", "Object", "Array",
                    "Dictionary"]

keyword :: Parser Token
keyword = Keyword <$> (genListParsers keywordsList <|> genListParsers builtInTypeList)

comment :: Parser Token
comment = Comment <$> ((:) <$> parseChar '#' <*> parseWhile (/='\n'))

seperatorList :: [String]
seperatorList = ["\n", "\r\n", " ", "\t", ";", ","]

seperator :: Parser Token
seperator = Seperator <$> genListParsers' seperatorList parseSequence

operatorList :: [String]
operatorList = ["->", "[", "]", ".", "(", ")", "{", "}", "~", "-", "*",
                "/", "%", "+", "-", "<<", ">>", "&", "^", "|", "<",
                ">", "==", "!=", ">=", "<=", "in", "!", "not", "and",
                "&&", "or", "||", "=", "+=", "-=", "*=", "/=", "%=",
                "&=", "!=", "<<=", ">>=", ":=", ":"]

operator :: Parser Token
operator = Operator <$> genListParsers' operatorList parseSequence

identifier :: Parser Token
identifier = Identifier <$> parseWhile p
    where
        p :: Char -> Bool
        p c = isAlphaNum c || c == '_' || c == '-' || c == '"'

--Generates a mega parser from a list of Strings 
genListParsers :: [String] -> Parser String
genListParsers = foldr ((<|>) . parseWord) (Parser (const Nothing))

--Same function as before, but takes a function which is being folded
genListParsers' :: [String] -> (String -> Parser String) -> Parser String
genListParsers' s f = foldr ((<|>) . f) (Parser (const Nothing)) s


chunk :: Parser Token
chunk = seperator <|> keyword <|> comment <|> operator <|> identifier


runAll :: String -> [Token]
runAll = runAll' [] 
    where
        runAll' acc [] = acc
        runAll' acc s =
            let
                (s', t) = f (runParser chunk s)
            in
                t : runAll' acc s'

        f (Just a) = a
        f Nothing = ("", Error)
