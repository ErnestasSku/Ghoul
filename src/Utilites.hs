module Utilites where

import Tokenizer (Token (..), runAll, Parser (runParser))
import Parsing.Parser
import GHC.IO
import Text.Parsec (parse)

-- next (x:xs) = xs
-- next [] = []

-- findNextToken :: [Token] -> Token -> Maybe [Token]
-- findNextToken (x:xs) t = if t == x then Just (x:xs) else findNextToken xs t
-- findNextToken [] _ = Nothing

-- findNextInLine :: [Token] -> Token -> Maybe [Token]
-- findNextInLine (x:xs) t
--   | x == Seperator "\n" = Nothing
--   | x == t = Just (x:xs)
--   | otherwise = findNextToken xs t
-- findNextInLine [] t = Nothing

findNextToken :: [Token] -> Token -> Maybe [Token]
findNextToken (x:xs) t = case x of
  Seperator "\n" -> Nothing 
  _ -> if x == t then Just (x:xs) else findNextToken xs t
  -- Identifier s -> undefined
  -- Keyword s -> undefined
  -- Seperator s -> undefined
  -- Operator s -> undefined
  -- Comment s -> undefined
  -- Error -> undefined
findNextToken [] _ = Nothing 


test :: [Token] -> Maybe Token
test (x:xs) = case x of
  Seperator s -> Nothing
  _  -> Just x 
test [] = Nothing 



--TODO: Debug/test functions. Delete when later.

mkt :: [Token]
mkt = let
    --Evil code, should never be used.
    a  = unsafePerformIO $ readFile "test//GdScript//test1.gd"
    in
        runAll a

mkt2 :: [Token]
mkt2 = let
    --Evil code, should never be used.
    a  = unsafePerformIO $ readFile "test//GdScript//test2.gd"
    in
        runAll a

mkt3 :: [Token]
mkt3 = let
    --Evil code, should never be used.
    a  = unsafePerformIO $ readFile "test//GdScript//test3.gd"
    in
        runAll a

mkt4 = do
  a <- readFile "test//GdScript//test1.gd"
  b <- readFile "test//GdScript//test2.gd"
  c <- readFile "test//GdScript//test3.gd"

  putStrLn $ readExpr a
  putStrLn "\n"
  putStrLn $ readExpr b
  putStrLn "\n"
  putStrLn $ readExpr c
  putStrLn "\n"


readExpr :: String -> String
readExpr input = case parse parseAll "test1" input of
    Left err -> "No match: \n" ++ show err
    Right val -> "Found value \n" ++ show val
