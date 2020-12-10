module Blocks where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Builder
import Crypto.Hash.SHA256

data Block = Block {index :: String, transactions :: String, timestamp :: Integer} deriving (Show)
--
computeHash :: Block -> String
computeHash Block {index=index, transactions=transactions, timestamp=timestamp} = unpack(toLazyByteString (byteStringHex ( hash (pack "Hello"))))