{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.ByteString.From (FromByteString(..), fromByteString) where

import           Control.Applicative                     ((<$>), (<|>))
import           Data.Attoparsec.ByteString              (parse)
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                         (ByteString)
import           Data.ByteString.Char8                   (empty, unpack)
import           Data.Int
import           Data.Text                               (Text)
import           Data.Text.Encoding                      (decodeUtf8)
import           Data.Word
import           Prelude                          hiding (takeWhile)

class FromByteString a where
    parser :: Parser a

fromByteString :: FromByteString a => ByteString -> Maybe a
fromByteString = either (const Nothing) Just . runParser parser

runParser :: Parser a -> ByteString -> Either String a
runParser p b = case (feed (parse p b) empty) of
    Done "" r  -> Right r
    Done t r   -> Left $ "Trailing input: " ++ show t
    Fail t _ e -> Left $ "Parsing failed before '" ++ show t ++ "': " ++ e
    Partial c  -> Left   "Unexpected result: Partial"

instance FromByteString ByteString where
    parser = takeByteString

instance FromByteString String where
    parser = unpack <$> takeByteString

instance FromByteString Text where
    parser = decodeUtf8 <$> takeByteString

instance FromByteString Int where
    parser = signed decimal <|> fail "Invalid Int"

instance FromByteString Int8 where
    parser = signed decimal <|> fail "Invalid Int8"

instance FromByteString Int16 where
    parser = signed decimal <|> fail "Invalid Int16"

instance FromByteString Int32 where
    parser = signed decimal <|> fail "Invalid Int32"

instance FromByteString Int64 where
    parser = signed decimal <|> fail "Invalid Int64"

instance FromByteString Word where
    parser = decimal <|> fail "Invalid Word"

instance FromByteString Word8 where
    parser = decimal <|> fail "Invalid Word8"

instance FromByteString Word16 where
    parser = decimal <|> fail "Invalid Word16"

instance FromByteString Word32 where
    parser = decimal <|> fail "Invalid Word32"

instance FromByteString Word64 where
    parser = decimal <|> fail "Invalid Word64"
