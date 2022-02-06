module Utilites
  ( fromASTtoString
  , (<->)
  , createOutputString
  , reconstructLine
  )
where
import Parsing.AST ( AST(..), (<!) )
import System.FilePath (splitPath, (</>))
import System.Console.Pretty

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


createOutputString :: [(Int, String)] -> FilePath -> [AST] -> [String]
createOutputString (x:xs) f a = (color Cyan "File: " ++ color Cyan f ++ color Green " | line :" ++ color Red (show (fst x)) ++ color White (reconstructLine (filter (<! fst x) a)) ++ color Yellow " | " ++ color Yellow (snd x) ++ "\n" ) : createOutputString xs f a
createOutputString [] _ _ = []


reconstructLine :: [AST] -> String
reconstructLine (x:xs) = " " ++ fromASTtoString x ++ reconstructLine xs
reconstructLine [] = []
