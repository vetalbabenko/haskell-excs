module BlockChain where

import Blocks
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.List as L
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Builder
import Crypto.Hash.SHA256
import JsonFunc
import System.IO.Unsafe
import Data.Time.Clock.POSIX

data BlockChain = BlockChain { unconfirmedTransactions:: [Block], chain :: [Block], genesisBlock :: Block, difficulty :: Int}

-- Calculate hash of new block until it satisfy condition (start with several repeated chararcter)
proofOfWork :: Block -> Int -> String
proofOfWork block difficulty = proofOfWorkRec block difficulty (computeHash block)

--Actual calculation of hash, until it satisfy condition
proofOfWorkRec:: Block -> Int -> String -> String
proofOfWorkRec block@Block {index = i, transactions = ts, timestamp = t, previousHash = pH, nonce = n} difficulty hash =
                            let prefix = replicate difficulty n
                            in if(L.isPrefixOf prefix hash) then hash
                                                          else proofOfWorkRec block difficulty (computeHash block)

--Computation of block hash
computeHash :: Block -> String
computeHash block = unpack $ toLazyByteString $ byteStringHex $ hash $ pack $ unpack $ A.encode block

--Creation of genesis block
genesis :: Block
genesis = Block {index = 0, transactions = [], timestamp = currTime, previousHash = "0", nonce = '0'}

--Help function of retrieving current time
currTime :: Int
currTime = round (unsafePerformIO getPOSIXTime) :: Int

--Help function of to retrieve last block
lastBlock :: BlockChain ->  Block
lastBlock bChain = head $ chain bChain

--Check if new block is valid: 
--  1) prevhash should match hash of previous block
--  2) hash of block should match of computed hash of this block
isValidProof :: Block -> String -> Int -> Bool
isValidProof block hash difficulty = if (L.isPrefixOf (replicate difficulty (nonce block)) hash && hash == computeHash block) then True
                                    else False

-- Adding no block to blockchain if it pass proof of work validation.
addNewBlock :: BlockChain -> Block -> String -> BlockChain
addNewBlock bChain block hash =
      let last = lastBlock bChain
          isPrevHashCorrect = computeHash last == previousHash block
          isValProof = isValidProof block hash (difficulty bChain)
      in if(isPrevHashCorrect && isValProof) then BlockChain {
          unconfirmedTransactions = [],
          chain = block : (chain bChain),
          genesisBlock = genesisBlock bChain,
          difficulty = difficulty bChain
          }
       else bChain


--If there are unconfirmed transaction, then it creates new block and try to brut force hash to this block.
mine :: BlockChain -> BlockChain
mine bChain@BlockChain {unconfirmedTransactions = uTs, chain = chain, genesisBlock = gB, difficulty = d} =
    if (length uTs == 0) then bChain
                         else let lastBlock = head chain
                                  block = Block {
                                          index = index lastBlock,
                                          transactions = uTs,
                                          timestamp = currTime,
                                          previousHash = computeHash lastBlock,
                                          nonce = nonce lastBlock
                                  }
                                  proof = proofOfWork block d
                                  in addNewBlock bChain block proof
