import Test.HUnit
import Data.Maybe
import QuickCheckTests (qcTests)
import Control.Monad (forM)
import Test.QuickCheck (quickCheck)
import HUnitTests (primaryTestCases)

main :: IO ()
main = do

    -- Quick Check Result
    putStrLn "QuickCheck"
    qcr <- forM qcTests quickCheck
    print qcr

    putStrLn "\nHUnit"
    count <- runTestTT primaryTestCases

    return ()