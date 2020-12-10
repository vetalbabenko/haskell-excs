module Blocks where

-- Representation of block in blockhain.
data Block = Block {index :: Integer, transactions :: [Block], timestamp :: Int, previousHash:: String, nonce :: Char} deriving (Show)


    