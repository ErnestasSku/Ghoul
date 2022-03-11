import Test.HUnit
import Data.Maybe
import QuickCheckTests (qcTests)
import Control.Monad (forM)
import Test.QuickCheck (quickCheck)

main :: IO ()
main = do

    -- Quick Check Result
    qcr <- forM qcTests quickCheck
    print qcr

    return ()