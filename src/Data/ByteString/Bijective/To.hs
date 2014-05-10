{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.Bijective.To (ToByteString(..)) where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy    as LB
import           Data.ByteString.Builder    (Builder)
import qualified Data.ByteString.Builder as Build
import qualified Data.ByteString.Char8   as C
import           Data.Int
import           Data.Word
import           Data.Text                  (Text)
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT


class ToByteString a where
    toByteString :: a -> ByteString

instance ToByteString ByteString where
    toByteString = id

instance ToByteString LB.ByteString where
    toByteString = LB.toStrict

instance ToByteString Text where
    toByteString = T.encodeUtf8

instance ToByteString LT.Text where
    toByteString = LB.toStrict . LT.encodeUtf8

instance ToByteString a => ToByteString [a] where
    toByteString = C.intercalate "," . map toByteString

instance ToByteString Bool where
    toByteString True  = "True"
    toByteString False = "False"

instance ToByteString Integer where
    toByteString = buildByteString . Build.integerDec

instance ToByteString Int8 where
    toByteString = buildByteString . Build.int8Dec

instance ToByteString Int16 where
    toByteString = buildByteString . Build.int16Dec

instance ToByteString Int32 where
    toByteString = buildByteString . Build.int32Dec

instance ToByteString Int64 where
    toByteString = buildByteString . Build.int64Dec

instance ToByteString Int where
    toByteString = buildByteString . Build.intDec

instance ToByteString Word8 where
    toByteString = buildByteString . Build.word8Dec

instance ToByteString Word16 where
    toByteString = buildByteString . Build.word16Dec

instance ToByteString Word32 where
    toByteString = buildByteString . Build.word32Dec

instance ToByteString Word64 where
    toByteString = buildByteString . Build.word64Dec

instance ToByteString Word where
    toByteString = buildByteString . Build.wordDec

instance ToByteString Float where
    toByteString = buildByteString . Build.floatDec

instance ToByteString Double where
    toByteString = buildByteString . Build.doubleDec

--- * Utility functions

buildByteString :: Builder -> ByteString
buildByteString = LB.toStrict . Build.toLazyByteString
