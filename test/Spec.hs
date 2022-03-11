import Test.HUnit
import Data.Maybe
import QuickCheckTests (qcTests)
import Control.Monad (forM, forM_)
import Test.QuickCheck (quickCheck)
import HUnitTests (primaryTestCases)

main :: IO ()
main = do

    putStr "QuickCheck\n"
    forM_ qcTests $ \(f, s) -> do
        putStr $ s ++ ": "
        quickCheck f

    putStrLn "\nHUnit"
    count <- runTestTT primaryTestCases

    return ()