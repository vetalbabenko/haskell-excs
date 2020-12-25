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
import Debug.Trace

data BlockChain = BlockChain { unconfirmedTransactions:: [String], chain :: [Block], difficulty :: Int}

initialBlockChain :: BlockChain
initialBlockChain = BlockChain {
          unconfirmedTransactions = [],
          chain = [genesis],
          difficulty = 1
          }

-- Calculate hash of new block until it satisfy condition (start with several repeated chararcter)
proofOfWork :: Block -> Int -> String
proofOfWork block difficulty = proofOfWorkRec block difficulty (computeHash block)

--Actual calculation of hash, until it satisfy condition
proofOfWorkRec:: Block -> Int -> String -> String
proofOfWorkRec block@Block {index = i, transactions = ts, timestamp = t, previousHash = pH, nonce = n} difficulty hash =
                            let prefix = L.intercalate  "" (replicate difficulty (show n))
                                updatedBlock = Block {index = succ i, transactions = ts, timestamp = t, previousHash = pH, nonce = n} 
                            in if(L.isPrefixOf prefix hash) then hash
                                                          else proofOfWorkRec updatedBlock difficulty (computeHash updatedBlock)

--Computation of block hash
computeHash :: Block -> String
computeHash block = unpack $ toLazyByteString $ byteStringHex $ hash $ pack $ unpack $ A.encode block

--Creation of genesis block
genesis :: Block
genesis = Block {index = 0, transactions = [], timestamp = currTime, previousHash = "0", nonce = 0}

--Help function of retrieving current time
currTime :: Int
currTime = round (unsafePerformIO getPOSIXTime) :: Int

--Help function of to retrieve last block
lastBlock :: BlockChain -> Block
lastBlock bChain = head $ chain bChain

resync :: BlockChain -> BlockChain
resync BlockChain {unconfirmedTransactions = uTs, chain = c, difficulty = d} =
                                let newChain = BlockChain {unconfirmedTransactions = uTs, chain = [last c], difficulty = d}
                                    resyncedChain = foldr (\b acc -> addNewBlock newChain b (proofOfWork b d)) newChain (L.init c)   
                                in BlockChain {unconfirmedTransactions = uTs, chain = chain resyncedChain, difficulty = d} 

-- Adding no block to blockchain if it pass proof of work validation.
addNewBlock :: BlockChain -> Block -> String -> BlockChain
addNewBlock bChain block hash =
      let last = lastBlock bChain
          isPrevHashCorrect = computeHash last == previousHash block
      in if(isPrevHashCorrect) then BlockChain {
          unconfirmedTransactions = [],
          chain = block : (chain bChain),
          difficulty = difficulty bChain
          }
       else bChain


--If there are unconfirmed transaction, then it creates new block and try to brut force hash to this block.
mine :: BlockChain -> (Maybe String, BlockChain)
mine bChain@BlockChain {unconfirmedTransactions = uTs, chain = chain, difficulty = d} =
    if (length uTs == 0) then (Nothing, bChain)
                         else let lastBlock = head chain
                                  block = Block {
                                          index = index lastBlock,
                                          transactions = uTs,
                                          timestamp = currTime,
                                          previousHash = computeHash lastBlock,
                                          nonce = nonce lastBlock
                                  }
                                  proof = proofOfWork block d
                                  in (Just proof, addNewBlock bChain block proof)

addNewTransaction :: String -> BlockChain -> BlockChain
addNewTransaction ts bChain@BlockChain {unconfirmedTransactions = uTs, chain = chain, difficulty = d} =
     BlockChain {
               unconfirmedTransactions = ts : uTs ,
               chain = chain,
               difficulty = d
               }
