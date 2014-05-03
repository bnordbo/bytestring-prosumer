{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.To (ToByteString(..)) where

import           Data.ByteString            (ByteString)
import           Data.ByteString.Lazy       (toStrict)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8   as C
import           Data.Int
import           Data.Word
import           Data.Text                  (Text)
import qualified Data.Text.Encoding      as T

class ToByteString a where
    toByteString :: a -> ByteString

instance ToByteString ByteString where
    toByteString = id

instance ToByteString String where
    toByteString = C.pack

instance ToByteString Text where
    toByteString = T.encodeUtf8

instance ToByteString Bool where
    toByteString True  = "True"
    toByteString False = "False"

instance ToByteString Int8 where
    toByteString = buildByteString . B.int8Dec

instance ToByteString Int16 where
    toByteString = buildByteString . B.int16Dec

instance ToByteString Int32 where
    toByteString = buildByteString . B.int32Dec

instance ToByteString Int64 where
    toByteString = buildByteString . B.int64Dec

instance ToByteString Int where
    toByteString = buildByteString . B.intDec

instance ToByteString Integer where
    toByteString = buildByteString . B.integerDec

instance ToByteString Word8 where
    toByteString = buildByteString . B.word8Dec

instance ToByteString Word16 where
    toByteString = buildByteString . B.word16Dec

instance ToByteString Word32 where
    toByteString = buildByteString . B.word32Dec

instance ToByteString Word64 where
    toByteString = buildByteString . B.word64Dec

instance ToByteString Word where
    toByteString = buildByteString . B.wordDec

instance ToByteString Float where
    toByteString = buildByteString . B.floatDec

instance ToByteString Double where
    toByteString = buildByteString . B.doubleDec

--- * Utility functions

buildByteString :: B.Builder -> ByteString
buildByteString = toStrict . B.toLazyByteString
