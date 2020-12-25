{-# LANGUAGE OverloadedStrings #-}
module ServerBlockchain where
import qualified BlockChain as B
import qualified Blocks as BL
import Web.Scotty
import Data.Monoid(mconcat)
import System.IO
import Control.Monad.Trans(liftIO,lift,MonadIO)
import Data.ByteString.Lazy.Char8 as C
import Data.IORef
import qualified Data.Text.Lazy as T
import qualified System.IO as SI
import qualified Data.List as L

-- Verify whether block was mined or there are no transactions in chain.
minedBlock :: Maybe String -> String
minedBlock (Just hash) = "Added new block with hash: " ++ hash
minedBlock _ = "No unconfirmed transaction, nothing to mined"

-- Main function, which start block chain.
runBlockChain :: IO ()
runBlockChain = do
                   ref <- newIORef (B.initialBlockChain)
                   C.putStrLn "Welcome to the Blockchain! What would you like to do today?"
                   C.putStrLn "Add New Block, Request to Mine, Resync Blockchain"
                   scotty 3000 $ do
                   -- Define endpoints, to interact with blockchain

                   -- Return all blocks in chain
                      get "/chain" $
                          do
                          liftIO $ C.putStrLn "Calling GET chain endpoint"
                          bChain <- liftIO $ readIORef ref
                          let chain = B.chain bChain
                          html $ mconcat [T.pack (L.intercalate ", " (Prelude.map show chain) ++ "\n") ]

                   -- Start mining block
                      get "/mine" $
                          do
                          liftIO $ C.putStrLn "Calling GET mine endpoint"
                          bChain <- liftIO $ readIORef ref
                          let tuple = B.mine bChain
                              minedHash = fst tuple
                              chain = snd tuple
                          liftIO $ writeIORef ref chain
                          html $ mconcat [T.pack (minedBlock minedHash ++ "\n") ]

                   -- Start resync of blockchain
                      get "/resync" $
                          do
                          liftIO $ C.putStrLn "Calling GET resync endpoint"
                          bChain <- liftIO $ readIORef ref
                          let newChain = B.resync bChain
                          liftIO $ writeIORef ref newChain
                          html $ mconcat ["Blockchain resync finished successfully \n"]

                   -- Add new unconfirmed transaction in block
                      post "/add_transaction" $
                          do
                          liftIO $ C.putStrLn "Calling POST add_transaction endpoint"
                          b <- body
                          bChain <- liftIO $ readIORef ref
                          let newChain = B.addNewTransaction (C.unpack b) bChain
                          liftIO $ writeIORef ref newChain
                          html $ mconcat [T.pack "\n" ]

                   -- Return all unconfirmed transactions in block
                      get "/unconfirmed_transaction" $
                          do
                          bChain <- liftIO $ readIORef ref
                          liftIO $ C.putStrLn "Calling GET unconfirmed_transaction endpoint"
                          html $ mconcat [T.pack (show (B.unconfirmedTransactions bChain) ++ "\n")]

