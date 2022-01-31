module Utilites (
    fromASTtoString
  , mkt5
) 

where

<<<<<<< HEAD
=======
import Tokenizer (Token (..), runAll, Parser (runParser), runnAll)
>>>>>>> parent of 7c5880b (Removed removed code.)
import Parsing.Parser
import GHC.IO
import Text.Parsec (parse)
import Parsing.AST

fromASTtoString :: AST -> String
fromASTtoString (Identifier s _) = s
fromASTtoString (Keyword s _) = s
fromASTtoString (Operator s _) = s
fromASTtoString (Comment s _) = s




-- mktT1 :: [Token]
mktT1 = let
    --Evil code, should never be used.
    a  = unsafePerformIO $ readFile "test//GdScript//test3.gd"
    in
        runParser runnAll a

-- mkt4 :: [Token]
-- mkt4 = let
--     --Evil code, should never be used.
--     a  = unsafePerformIO $ readFile "test//GdScript//test3.gd"
--     in 
--         parse parseAll "testFile" a >>=
--           -- Left err -> show err
--           -- Right val -> show val

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


bb :: String -> [AST]
bb input = case parse parseAll "a" input of
    Left err -> []
    Right val -> val

mkt5 =
  let
    a  = unsafePerformIO $ readFile "test//GdScript//test1.gd"
  in
    bb a
    
