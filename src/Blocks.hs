module Blocks where

data Block = Block {index :: Integer, transactions :: [Block], timestamp :: Int, previousHash:: String, nonce :: Char} deriving (Show)


    