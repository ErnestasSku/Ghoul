module TypeChecker where

import Tokenizer (Token (Keyword, Operator, Seperator), )
import Utilites (findNextToken)
import Data.Maybe (isJust)


--Works fine. Although, it does not look too elegant.
checkTypes :: [Token] -- Input list of tokens 
  -> [(Int, String)]  -- Output of Line number and String. Todo: rewrite this into a new datatype which would allow for more flexibility 
checkTypes t= reverse $ checkTypesFunc t 1 []
    where
    checkTypesFunc :: [Token] -> Int -> [(Int, String)] -> [(Int, String)]
    checkTypesFunc y@(x:xs) l acc = case x of
        Seperator "\n" -> checkTypesFunc xs (l + 1) acc
        Keyword "var" ->
            let
                opList = [Operator ":=", Operator ":"]

                fList = map (findNextToken y) opList

            in
                if any isJust fList
                then checkTypesFunc xs l acc
                else checkTypesFunc xs l $ (l, "Variable is not staticly typed") : acc
        Keyword "func" ->
            let 
                opList = [Operator "->"]
                fList = map (findNextToken y) opList
            in
                if any isJust fList
                then checkTypesFunc xs l acc
                else checkTypesFunc xs l $ (l, "Function is not staticly typed") : acc
        _ -> checkTypesFunc xs l acc
    checkTypesFunc [] _ acc = acc

