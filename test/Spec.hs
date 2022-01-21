import Test.HUnit
import Tokenizer 
import Data.Maybe

-- sc = smallest (parser) component
sc_char_accept1 = TestCase $ assertBool "Char parser 1: " (isJust $ runParser (parseChar 'x') "x")  
sc_char_fail1 = TestCase $ assertBool "Char parser 2: " (isNothing $ runParser (parseChar 'y') "x"  )

sc_string_accept1 = TestCase $ assertBool "String parser 1: " (isJust $ runParser (parseSequence "test") "test")  
sc_string_fail1 = TestCase $ assertBool "String parser 2: " (isNothing $ runParser (parseSequence "test") "xest") 
sc_string_accept2 = TestCase $ assertBool "String parser 3: " (isJust $ runParser (parseSequence "test") "test test")  
sc_string_fail2 = TestCase $ assertBool "String parser 4: " (isNothing $ runParser (parseSequence "test") "123xest") 


smallestComponentTests = TestList [TestLabel "Char 1" sc_char_accept1,
                                   TestLabel "Char 2" sc_char_fail1,
                                   TestLabel "String 1" sc_string_accept1,
                                   TestLabel "String 2" sc_string_fail1,
                                   TestLabel "String 3" sc_string_accept2,
                                   TestLabel "String 4" sc_string_fail2
                                   ]

parseTest1 = TestCase (do
    file <- readFile "test//GdScript//test1.gd"
    let tokenized = runAll file
    assertEqual "File test1 tokenization errors: " (length $ filter (== Error) tokenized) 0
    )

parseTest2 = TestCase (do
    file <- readFile "test//GdScript//test1.gd"
    let tokenized = runAll file
    assertEqual "File test1 tokenization errors: " (length $ filter (== Error) tokenized) 0
    )

parseTest3 = TestCase (do
    file <- readFile "test//GdScript//test1.gd"
    let tokenized = runAll file
    assertEqual "File test1 tokenization errors: " (length $ filter (== Error) tokenized) 0
    )

fileTokenization = TestList[TestLabel "Token 1" parseTest1,
                            TestLabel "Token 2" parseTest2,
                            TestLabel "Token 3" parseTest3]

main :: IO ()
main = do
    counts <- runTestTT smallestComponentTests
    counts <- runTestTT fileTokenization

    return ()