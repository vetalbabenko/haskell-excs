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

proofOfWork :: Block -> Int -> String
proofOfWork block difficulty = proofOfWorkRec block difficulty (computeHash block)

proofOfWorkRec:: Block -> Int -> String -> String
proofOfWorkRec block@Block {index = i, transactions = ts, timestamp = t, previousHash = pH, nonce = n} difficulty hash =
                            let prefix = replicate difficulty n
                            in if(L.isPrefixOf prefix hash) then hash
                                                          else proofOfWorkRec block difficulty (computeHash block)

computeHash :: Block -> String
computeHash block = unpack $ toLazyByteString $ byteStringHex $ hash $ pack $ unpack $ A.encode block

genesis :: Block
genesis = Block {index = 0, transactions = [], timestamp = currTime, previousHash = "0", nonce = '0'}

currTime :: Int
currTime = round (unsafePerformIO getPOSIXTime) :: Int

lastBlock :: BlockChain ->  Block
lastBlock bChain = head $ chain bChain

isValidProof :: Block -> String -> Int -> Bool
isValidProof block hash difficulty = if (L.isPrefixOf (replicate difficulty (nonce block)) hash && hash == computeHash block) then True
                                    else False

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
