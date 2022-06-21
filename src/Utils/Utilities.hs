module Utils.Utilities
  ( fromASTtoString
  , (<->)
  , createOutputString
  , reconstructLine
  , getYesNo
  , ToString(..)
  )
where
import Parsing.AST ( AST(..), (<!) )
import System.FilePath (splitPath, (</>))

{-

-}
class ToString a where
  toString :: a -> String
  toStringMulti :: a -> String -> String
  toStringMulti a _ = toString a  


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


-- | gets either Yes or No.
--  This is used to get around windows bug/requirement to press enter
-- After each char input (unlike behavior in linux)
getYesNo :: IO Bool
getYesNo = do
  c <- getChar
  case c of
    'Y' -> return True
    'y' -> return True
    'N' -> return False
    'n' -> return False
    _ -> getYesNo