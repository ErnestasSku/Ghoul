module Utilites
  ( fromASTtoString
  , (<->)
  , createOutputString
  , reconstructLine
  )
where
import Parsing.AST ( AST(..), (<!) )
import System.FilePath (splitPath, (</>))


-- | Extracts the String from AST type
fromASTtoString :: AST -> String
fromASTtoString (Identifier s _) = s
fromASTtoString (Keyword s _) = s
fromASTtoString (Operator s _) = s
fromASTtoString (Comment s _) = "#" ++ s

-- | Prints file path, excluding the base path 
(<->) :: FilePath -> FilePath -> FilePath
base <-> file =
  let
    b' = splitPath base
    f' = splitPath file

    f = drop (length b') f'
  in
    foldr (</>) "" f


-- createOutputString :: [[(Int, String)]] -> FilePath ->  [AST] -> [String]
-- createOutputString ([]:xs) f a = createOutputString xs f a
-- -- createOutputString (x:xs) f a = (f ++ "| line: " ++ show (fst $ head x) ++ reconstructLine a) : createOutputString xs f a
-- createOutputString (x:xs) f a = undefined 
-- createOutputString [] _  _= []

createOutputString :: [(Int, String)] -> FilePath -> [AST] -> [String]
createOutputString (x:xs) f a = ("File: " ++ f ++ " | line : " ++ show (fst x) ++ reconstructLine (filter (<! fst x) a) ++ " - " ++ snd x ++ "\n" ) : createOutputString xs f a
createOutputString [] _ _ = []


reconstructLine :: [AST] -> String
reconstructLine (x:xs) = " " ++ fromASTtoString x ++ reconstructLine xs
reconstructLine [] = []
