module Blocks where

-- Representation of block in blockhain.
data Block = Block {index :: Integer, transactions :: [String], timestamp :: Int, previousHash:: String, nonce :: Int} deriving (Show)


    