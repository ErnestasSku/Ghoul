{-
  NOTE: in these QuickCheck tests I generate strings and try to parse them.
  This is not the best solution, a better one would be to generate an AST and
  then convert this AST to string and parse it again and check it with generated AST.

  At the moment I'm not sure whether I have a good way of making a Text/String from AST
  and also there might be some problems with generating position of tokens for AST.
-}

module QuickCheckTests
  ( qcTests,
  )
where

import Data.Either
import Data.List
import Parsing.Internal.GdKeywords
import Parsing.Internal.ParserInternal
import Test.QuickCheck
import Text.Parsec (parse)

{-
  Note:
  g... stands for GdScript
  a... stands for Agnostic
-}

-- GdScript keyword
gKeywordGen :: Gen String
gKeywordGen = elements keywordsList

-- GdScript types
gBuiltInTypes :: Gen String
gBuiltInTypes = elements builtInTypeList

-- GdScript operator
gOperators :: Gen String
gOperators = elements operatorList

-- Agnostic IDentifier
aID :: Gen String
aID = vectorOf 8 (elements viableChars)
  where
    viableChars = ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']

-- GdScript var expression
gVarExpr :: Gen String
gVarExpr = do
  v <- viableVar
  name <- aID
  nws <- choose (0, 1) :: Gen Int
  ws <- vectorOf nws $ elements [" "]
  op <- viableOp
  id <- aID
  nws' <- choose (0, 1) :: Gen Int
  ws' <- vectorOf nws' $ elements [" "]

  return $ v ++ " " ++ name ++ concat ws ++ op ++ concat ws' ++ id
  where
    viableVar = elements ["export var", "var"]
    viableOp = elements ["=", ":="]

-- GdScript func expression
gFuncExpr :: Gen String
gFuncExpr = do
  let func = "func "
  n <- randomNum 4 8
  funcName <- vectorOf n (elements viableFuncName)
  args' <- argsG
  let args = intercalate "," args'
  end <- endG

  return $ func ++ funcName ++ "(" ++ args ++ ")" ++ end
  where
    viableFuncName = ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_-"
    randomNum x y = choose (x, y)
    endG = oneof [elements [":"], typedEndG]
    typedEndG :: Gen String
    typedEndG = do
      type' <- elements builtInTypeList
      return $ " -> " ++ type' ++ ":"

    argsG :: Gen [String]
    argsG = do
      n <- randomNum 0 3

      vectorOf n $ oneof [typedArg, unTypedArg]

    unTypedArg = aID

    typedArg :: Gen String
    typedArg = do
      name <- aID
      type' <- elements builtInTypeList
      return $ name ++ ": " ++ type'

--All properties
qcTests :: [Property]
qcTests = [prop_keyword, prop_builtIn, prop_Operators, prop_id, prop_varExpr, prop_funcExpr]

-- Properties
prop_keyword :: Property
prop_keyword = forAllShrink gKeywordGen shrink $ \gk ->
  case parse keyword "" gk of
    Left _ -> False
    Right _ -> True

prop_builtIn :: Property
prop_builtIn = forAllShrink gBuiltInTypes shrink $ \gb ->
  case parse keyword' "" gb of
    Left _ -> False
    Right _ -> True

prop_Operators :: Property
prop_Operators = forAllShrink gOperators shrink $ \go ->
  case parse operator "" go of
    Left pe -> False
    Right ast -> True

prop_id :: Property
prop_id = forAllShrink aID shrink $ \id ->
  case parse identifier "" id of
    Left pe -> False
    Right ast -> True

prop_varExpr :: Property
prop_varExpr = forAllShrink gVarExpr shrink $ \id ->
  case parse parseAll "" id of
    Left pe -> False
    Right ast -> True

prop_funcExpr :: Property
prop_funcExpr = forAllShrink gFuncExpr shrink $ \id ->
  case parse parseAll "" id of
    Left pe -> False
    Right ast -> True
