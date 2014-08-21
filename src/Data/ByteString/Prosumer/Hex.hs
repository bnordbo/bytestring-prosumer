{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.ByteString.Prosumer.Hex (Hex(..)) where

import Control.Applicative
import Data.Attoparsec.ByteString       (parse)
import Data.Attoparsec.ByteString.Char8
import Data.Bits                        (Bits)
import Data.ByteString.Char8            (pack)
import Data.ByteString.Prosumer.From    (FromByteString(..))
import Data.ByteString.Prosumer.To      (ToByteString(..))
import Numeric                          (showHex)
import Text.Printf                      (PrintfArg)


newtype Hex a = Hex { fromHex :: a }
    deriving ( Bits
             , Bounded
             , Enum
             , Eq
             , Integral
             , Num
             , Ord
             , PrintfArg
             , Read
             , Real
             , Show
             )

instance (Bits a, Integral a, Real a) => FromByteString (Hex a) where
    parser = optional (char '0' *> satisfy (`elem` "xX")) >> hexadecimal

instance (Integral a, Show a) => ToByteString (Hex a) where
    toByteString (Hex i) = pack $ showHex i ""
