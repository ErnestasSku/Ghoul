module Utils.Utilities
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


createOutputString :: [(Int, String)] -- [(Line in code, found warning)] 
  -> FilePath -- File 
  -> [AST] -- Full AST 
  -> [(String, String, String, String)] -- (File path and name, line number, code excerpt, found warning)
-- createOutputString (x:xs) f a = (color Cyan "File: " ++ color Cyan f ++ color Green " | line :" ++ color Red (show (fst x)) ++ color White (reconstructLine (filter (<! fst x) a)) ++ color Yellow " | " ++ color Yellow (snd x) ++ "\n" ) : createOutputString xs f a
createOutputString (x:xs) f a = (f, show (fst x), reconstructLine (filter (<! fst x) a), snd x) : createOutputString xs f a
createOutputString [] _ _ = []

-- #TODO: make a show instance for AST
reconstructLine :: [AST] -> String
reconstructLine (x:xs) = " " ++ fromASTtoString x ++ reconstructLine xs
reconstructLine [] = []
