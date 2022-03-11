module HUnitTests (primaryTestCases) where

import Control.Monad (forM)
import GHC.IO (unsafePerformIO)
import Parsing.Internal.ParserInternal
import Rules.CommentChecker (commentCheck)
import Rules.Rules (Rule (ProperComments, StaticTypes))
import Rules.TypeChecker (typeCheck)
import Test.HUnit
import Text.Parsec.Prim (parse)

numberOfFiles :: Int
numberOfFiles = 3

numberOfMistakes :: [Int]
numberOfMistakes = [4, 0, 14]

testFilePattern :: Int -> String
testFilePattern x = "test//GdScript//test" ++ show x ++ ".gd"

testData :: [(String, Int)]
testData = flip zip numberOfMistakes $ map testFilePattern [1 .. numberOfFiles]

--NOTE: Evil code. unsafe IO. Find a way to get rid of this Evil code.
filePartialTestFunction :: String -> IO Bool -> Test
filePartialTestFunction txt bol = TestCase $ assertBool txt (unsafePerformIO bol)

parseFile :: FilePath -> Int -> IO Bool
parseFile file x = do
  fl <- readFile file
  let toks = parse parseAll file fl

  case toks of
    Left pe -> return False
    Right asts -> do
      let rules = [typeCheck, commentCheck]

      let mapped = map ($ asts) rules
      let collapsed = concat mapped

      return $ length collapsed == x

f :: (String, Int) -> Test
f (txt, n) = filePartialTestFunction txt (parseFile txt n)

primaryTestCases = TestList $ map f testData