{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.From (FromByteString(..), fromByteString) where

import           Control.Applicative
import           Control.Error
import           Data.Attoparsec.ByteString          (parse)
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Char8            as C
import           Data.Either

class FromByteString a where
    parser :: Parser a

fromByteString :: FromByteString a => ByteString -> Maybe a
fromByteString = either (const Nothing) Just . runParser parser

runParser :: Parser a -> ByteString -> Either String a
runParser p b = case (feed (parse p b) C.empty) of
    Done "" r  -> Right r
    Done t r   -> Left $ "Trailing input: " ++ show t
    Fail t _ e -> Left $ "Parsing failed before '" ++ show t ++ "': " ++ e
    Partial c  -> Left   "Unexpected result: Partial"

instance FromByteString Int where
    parser = signed decimal <|> fail "Invalid Int"
