{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.ByteString.Prosumer.From (FromByteString(..), fromByteString) where

import Control.Applicative              ((<$>), (*>), (<|>), optional)
import Data.Attoparsec.ByteString       (parse)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString                  (ByteString)
import Data.ByteString.Char8            (empty, unpack)
import Data.Int
import Data.Maybe                       (isJust)
import Data.Text                        (Text)
import Data.Text.Encoding               (decodeUtf8')
import Data.Word
import GHC.Float                        (double2Float)


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

instance FromByteString Text where
    parser = takeByteString
         >>= either (const $ fail "invalid UTF8") return . decodeUtf8'

instance FromByteString a => FromByteString [a] where
    parser = parseList

instance FromByteString Bool where
    parser = satisfy (`elem` "tT") *> "rue"  *> return True
         <|> satisfy (`elem` "fF") *> "alse" *> return False
         <|> fail "invalid Bool"

instance FromByteString Integer where
    parser = signed decimal <|> fail "Invalid Integer"

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

instance FromByteString Float where
    parser = double2Float <$> signed double <|> fail "Invalid Float"

instance FromByteString Double where
    parser = signed double <|> fail "Invalid Double"

--- * Helpers

parseList :: FromByteString a => Parser [a]
parseList = atEnd >>= \e ->
    if e then return []
         else reverse <$> go []
  where
    go acc = do
        x <- takeTill (== ',')
        v <- case runParser parser x of
                 Left e  -> fail e
                 Right a -> return a
        c <- optional (char ',')
        e <- atEnd
        case (e, isJust c) of
            (True,  True)  -> fail "trailing comma"
            (True,  False) -> return (v:acc)
            (False, True)  -> go (v:acc)
            (False, False) -> fail "missing comma"
