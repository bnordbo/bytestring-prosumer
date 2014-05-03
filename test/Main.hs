import           Control.Applicative
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.ByteString.From
import           Data.ByteString.To
import           Data.Char
import           Data.Int
import           Data.String
import           Data.Text                (Text)
import qualified Data.Text             as T
import           Data.Word
import           Test.Tasty
import           Test.Tasty.QuickCheck

main = defaultMain tests

tests :: TestTree
tests = testGroup "Bijective"
    [ testProperty "ByteString" $ \x ->
          fromByteString (toByteString x) == Just (x :: ByteString)
    , testProperty "String" $ \x ->
          fromByteString (toByteString x) == Just (x :: String)
    , testProperty "Text" $ \x ->
          fromByteString (toByteString x) == Just (x :: Text)
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
            [ choose (     0,   36095 )
            , choose ( 57344,   65533 )
            , choose ( 65536, 1114111 )
            ]
