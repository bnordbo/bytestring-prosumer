import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.ByteString.From
import Data.ByteString.To

main = defaultMain tests

tests :: TestTree
tests = testGroup "Bijective"
    [ QC.testProperty "Int" $ \x -> fromByteString (toByteString x) == Just (x :: Int)
    ]
