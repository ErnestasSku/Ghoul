{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Utils.FileUtilities
  ( ghoulFileRules,
    getRecursivePaths,
    predicateFind,
    findGdFiles,
    findGdFiles',
    ghoulFileNameExtension,
    ghoulFileExists,
    writeNewGhoulFile,
  )
where

-- import Rules.OldRules (RuleQuestionnaire)
import Rules.Rules
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory, getAccessTime)
import System.FilePath (takeExtension, (</>))
import Control.Monad (forM)
import Parsing.GhoulFile (parseRules, parseStyle)
import PrettyPrint.Styles (OutputStyle)
import Utils.Utilities (ToString(..))

-- # Proposal: if show instance for Rule/CompoundRule or Style would need to change,
-- it can be moved into GhoulWritable instance instead 
class GhoulWritable a where
  showGhoulSingle :: a -> String
  showGhoulList :: [a] -> String


instance GhoulWritable CompoundRule where
  showGhoulSingle x = "\t" ++ show x ++ "\n"
  showGhoulList l = "[Rules]\n" ++  foldr ((<>) . showGhoulSingle) "" l

instance GhoulWritable OutputStyle where
  showGhoulSingle x = "[Style]\n\t" ++ toStringMulti x "\n\t"
  showGhoulList = error "Output style is a record and should not be used as a list"

ghoulFileNameExtension :: String
ghoulFileNameExtension = "rules.ghoul"

--This is a very often reoccurring pattern in code
getCurrentGhoulDirectory :: IO FilePath
getCurrentGhoulDirectory = getCurrentDirectory >>= \x -> return $ x </> ghoulFileNameExtension

-- | Reads the rules.ghoul file
ghoulFileRules :: IO [CompoundRule]
ghoulFileRules = do
  ghoulF <- getCurrentGhoulDirectory
  input <- readFile ghoulF 
  let rules = parseRules input ghoulFileNameExtension
  print rules
  case rules of
    Left err -> return []
    Right val -> return val

ghoulFileStyle :: IO (Maybe OutputStyle)
ghoulFileStyle = do
  ghoulF <- getCurrentGhoulDirectory
  input <- readFile ghoulF
  let style = parseStyle input ghoulFileNameExtension
  print style
  case style of
    Left err -> return Nothing
    Right val -> return $ Just val


-- | Recursively finds all files
getRecursivePaths :: FilePath -> IO [FilePath]
getRecursivePaths topPath = do
  names <- listDirectory topPath
  paths <- forM names $ \name -> do
    let path = topPath </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursivePaths path
      else return [path]
  return (concat paths)

predicateFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
predicateFind p path = do
  names <- getRecursivePaths path
  return $ filter p names

findGdFiles :: FilePath -> IO [FilePath]
findGdFiles = predicateFind (\p -> takeExtension p == ".gd")

findGdFiles' :: IO [FilePath]
findGdFiles' = do
  cwd <- getCurrentDirectory
  predicateFind (\p -> takeExtension p == ".gd") cwd

ghoulFileExists :: IO Bool
ghoulFileExists = do
  gh <- getCurrentGhoulDirectory
  doesFileExist gh

writeNewGhoulFile :: (GhoulWritable a, Show a, GhoulWritable b, Show b) => 
  [a] -- ^ A list of rules 
  -> b -- ^ Output Style
  -> IO ()
writeNewGhoulFile r s = do
  gh <- getCurrentGhoulDirectory
  writeFile gh $ showGhoulList r <> showGhoulSingle s
