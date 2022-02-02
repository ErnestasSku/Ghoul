module Rules.CommentChecker where
import Parsing.AST

commentCheck :: [AST] -> [(Int, String)]
commentCheck (x:xs) = case x of
    Comment s (line, col) -> 
        if head s == ' ' then commentCheck xs
        else (line, "A proper comment should have a space at the begining") : commentCheck xs
    _ -> commentCheck xs
commentCheck [] = []