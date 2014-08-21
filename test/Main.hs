{-# LANGUAGE FlexibleInstances #-}

import           Control.Applicative
import           Data.ByteString              (ByteString)
import           Data.ByteString.Prosumer
import qualified Data.ByteString.Char8     as C
import           Data.Char
import           Data.Int
import           Data.String
import           Data.Text                     (Text)
import qualified Data.Text                  as T
import           Data.Word
import           Test.QuickCheck               (Gen)
import           Test.Tasty
import           Test.Tasty.QuickCheck


main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [ prosumer, relaxedInput ]

prosumer :: TestTree
prosumer = testGroup "Prosumer"
    [ testProperty "ByteString" $ \x ->
          fromByteString (toByteString x) == Just (x :: ByteString)
    , testProperty "Text" $ \x ->
          fromByteString (toByteString x) == Just (x :: Text)
    , testProperty "[a]" $ \x ->
          fromByteString (toByteString x) == Just (x :: [Bool])
    , testProperty "Bool" $ \x ->
          fromByteString (toByteString x) == Just (x :: Bool)
    , testProperty "Integer" $ \x ->
          fromByteString (toByteString x) == Just (x :: Integer)
    , testProperty "Int" $ \x ->
          fromByteString (toByteString x) == Just (x :: Int)
    , testProperty "Int8" $ \x ->
          fromByteString (toByteString x) == Just (x :: Int8)
    , testProperty "Int16" $ \x ->
          fromByteString (toByteString x) == Just (x :: Int16)
    , testProperty "Int32" $ \x ->
          fromByteString (toByteString x) == Just (x :: Int32)
    , testProperty "Int64" $ \x ->
          fromByteString (toByteString x) == Just (x :: Int64)
    , testProperty "Word" $ \x ->
          fromByteString (toByteString x) == Just (x :: Word)
    , testProperty "Word8" $ \x ->
          fromByteString (toByteString x) == Just (x :: Word8)
    , testProperty "Word16" $ \x ->
          fromByteString (toByteString x) == Just (x :: Word16)
    , testProperty "Word32" $ \x ->
          fromByteString (toByteString x) == Just (x :: Word32)
    , testProperty "Word64" $ \x ->
          fromByteString (toByteString x) == Just (x :: Word64)
    , testProperty "Float" $ \x ->
          fromByteString (toByteString x) == Just (x :: Float)
    , testProperty "Double" $ \x ->
          fromByteString (toByteString x) == Just (x :: Double)
    , testProperty "Hex" $ \x ->
          fromByteString (toByteString x) == Just (x :: Hex Int)
    ]

relaxedInput :: TestTree
relaxedInput = testGroup "Relaxed input rules"
    [ testProperty "Less case sensitive Bool input" $ \(BoolStr s) ->
          fromByteString s == Just (if C.head s `elem` "tT" then True else False)
    ]

instance Arbitrary ByteString where
    arbitrary = C.pack <$> listOf (choose ('\0', '\255'))

newtype UTF8 a = UTF8 a

instance Arbitrary Text where
    arbitrary = do
        UTF8 c <- arbitrary
        return $ T.pack c

instance IsString a => Arbitrary (UTF8 a) where
    arbitrary = UTF8 . fromString . map chr <$> listOf utf8
      where
        utf8 = oneof
            [ choose (     0,     191 )
            , choose (   194,     244 )
            , choose (   256,   55295 )
            , choose ( 57343, 1114111 )
            ]

newtype BoolStr = BoolStr ByteString
    deriving Show

instance Arbitrary BoolStr where
    arbitrary = BoolStr . C.pack <$>
        elements [ "true", "True", "false", "False" ]

instance Arbitrary (Hex Int) where
    arbitrary = Hex <$> suchThat arbitrary (>= 0)
