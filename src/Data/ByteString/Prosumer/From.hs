-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.Prosumer.From
    ( FromByteString (..)
    , fromByteString
    , runParser
    ) where

import           Control.Applicative
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8        (signed, decimal, double)
import           Data.ByteString                         (ByteString, elem)
import qualified Data.ByteString.Lazy                 as LB
import           Data.Int
import           Data.Maybe                              (isJust)
import           Data.Text                               (Text)
import           Data.Text.Encoding                      (decodeUtf8')
import qualified Data.Text.Lazy                       as LT
import qualified Data.Text.Lazy.Encoding              as LT
import           Data.Word
import           GHC.Float                               (double2Float)
import           Prelude                          hiding (elem)


-- | Parse 'ByteString's.
class FromByteString a where
    parser :: Parser a

fromByteString :: FromByteString a => ByteString -> Maybe a
fromByteString = either (const Nothing) Just . runParser parser

runParser :: Parser a -> ByteString -> Either String a
runParser p b = case feed (parse p b) "" of
    Done ""  r -> Right r
    Done _   _ -> Left "Trailing input"
    Fail _ _ m -> Left m
    Partial _  -> Left "Unexpected result: Partial"

-----------------------------------------------------------------------------
-- Instances

instance FromByteString ByteString where
    parser = takeByteString

instance FromByteString LB.ByteString where
    parser = takeLazyByteString

-- | A (flat) comma-separated list of values without spaces.
instance FromByteString a => FromByteString [a] where
    parser = parseList

-- | UTF-8 is assumed as encoding format.
instance FromByteString Text where
    parser = takeByteString >>= text

-- | UTF-8 is assumed as encoding format.
instance FromByteString LT.Text where
    parser = takeLazyByteString >>= ltext

instance FromByteString Bool where
    parser =
        satisfy (`elem` "tT") *> string "rue"  *> pure True  <|>
        satisfy (`elem` "fF") *> string "alse" *> pure False <|>
        fail "Invalid Bool"

instance FromByteString Float where
    parser = double2Float <$> signed double <|> fail "Invalid Float"

instance FromByteString Double where
    parser = signed double <|> fail "Invalid Double"

-- | (signed) decimal literals
-- (see "Data.ByteString.From.Hex" for hexadecimal parsing)
instance FromByteString Integer where
    parser = signed decimal <|> fail "Invalid Integer"

-- | (signed) decimal literals
-- (see "Data.ByteString.From.Hex" for hexadecimal parsing)
instance FromByteString Int where
    parser = signed decimal <|> fail "Invalid Int"

-- | (signed) decimal literals
-- (see "Data.ByteString.From.Hex" for hexadecimal parsing)
instance FromByteString Int8 where
    parser = signed decimal <|> fail "Invalid Int8"

-- | (signed) decimal literals
-- (see "Data.ByteString.From.Hex" for hexadecimal parsing)
instance FromByteString Int16 where
    parser = signed decimal <|> fail "Invalid Int16"

-- | (signed) decimal literals
-- (see "Data.ByteString.From.Hex" for hexadecimal parsing)
instance FromByteString Int32 where
    parser = signed decimal <|> fail "Invalid Int32"

-- | (signed) decimal literals
-- (see "Data.ByteString.From.Hex" for hexadecimal parsing)
instance FromByteString Int64 where
    parser = signed decimal <|> fail "Invalid Int64"

-- | (signed) decimal literals
-- (see "Data.ByteString.From.Hex" for hexadecimal parsing)
instance FromByteString Word where
    parser = signed decimal <|> fail "Invalid Word"

-- | (signed) decimal literals
-- (see "Data.ByteString.From.Hex" for hexadecimal parsing)
instance FromByteString Word8 where
    parser = signed decimal <|> fail "Invalid Word8"

-- | (signed) decimal literals
-- (see "Data.ByteString.From.Hex" for hexadecimal parsing)
instance FromByteString Word16 where
    parser = signed decimal <|> fail "Invalid Word16"

-- | (signed) decimal literals
-- (see "Data.ByteString.From.Hex" for hexadecimal parsing)
instance FromByteString Word32 where
    parser = signed decimal <|> fail "Invalid Word32"

-- | (signed) decimal literals
-- (see "Data.ByteString.From.Hex" for hexadecimal parsing)
instance FromByteString Word64 where
    parser = signed decimal <|> fail "Invalid Word64"

-----------------------------------------------------------------------------
-- Implementation Helpers

parseList :: FromByteString a => Parser [a]
parseList = atEnd >>= \e ->
    if e then return []
         else reverse <$> go []
  where
    go acc = do
        x <- takeTill (== 0x2C)
        v <- case runParser parser x of
                Left  s -> fail s
                Right a -> return a
        c <- optional (word8 0x2C)
        e <- atEnd
        case (e, isJust c) of
            (True,  True)  -> fail "trailing comma"
            (True,  False) -> return (v:acc)
            (False, True)  -> go (v:acc)
            (False, False) -> fail "missing comma"

text :: ByteString -> Parser Text
text = either (fail . ("Invalid UTF-8: " ++) . show) return . decodeUtf8'

ltext :: LB.ByteString -> Parser LT.Text
ltext = either (fail . ("Invalid UTF-8: " ++) . show) return . LT.decodeUtf8'
