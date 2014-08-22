-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.ByteString.Prosumer.Hex where

import Control.Applicative
import Data.Attoparsec.ByteString       (parse, word8)
import Data.Attoparsec.ByteString.Char8
import Data.Bits                        (Bits)
import Data.ByteString.Char8            (pack)
import Data.ByteString.Prosumer.From
import Data.ByteString.Prosumer.To
import Numeric                          (showHex)
import Text.Printf                      (PrintfArg)


-- | Newtype wrapper to parse integral numbers in hexadecimal
-- format, optionally with a @0x@ or @0X@ prefix.
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

instance (Integral a, Bits a) => FromByteString (Hex a) where
    parser = Hex <$> signed (optional prefix *> hexadecimal)
      where
        prefix = word8 0x30 *> satisfy (`elem` "xX")

instance (Integral a, Show a) => ToByteString (Hex a) where
    toByteString (Hex i) = pack $ showHex i ""
