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

minedBlock :: Maybe String -> String
minedBlock (Just hash) = "Added new block with hash: " ++ hash
minedBlock _ = "No unconfirmed transaction, nothing to mined"


runBlockChain :: IO ()
runBlockChain = do
                   ref <- newIORef (B.initialBlockChain)
                   scotty 3000 $ do
                      get "/chain" $
                          do
                          liftIO $ C.putStrLn "Calling GET chain endpoint"
                          bChain <- liftIO $ readIORef ref
                          let chain = B.chain bChain
                          html $ mconcat [T.pack (L.intercalate ", " (Prelude.map show chain) ++ "\n") ]

                      get "/mine" $
                          do
                          liftIO $ C.putStrLn "Calling GET mine endpoint"
                          bChain <- liftIO $ readIORef ref
                          let tuple = B.mine bChain
                              minedHash = fst tuple
                              chain = snd tuple
                          liftIO $ writeIORef ref chain
                          html $ mconcat [T.pack (minedBlock minedHash ++ "\n") ]

                      post "/add_transaction" $
                          do
                          liftIO $ C.putStrLn "Calling POST add_transaction endpoint"
                          b <- body
                          bChain <- liftIO $ readIORef ref
                          let newChain = B.addNewTransaction (C.unpack b) bChain
                          liftIO $ writeIORef ref newChain
                          html $ mconcat [T.pack "\n" ]

                      get "/unconfirmed_transaction" $
                          do
                          bChain <- liftIO $ readIORef ref
                          liftIO $ C.putStrLn "Calling GET unconfirmed_transaction endpoint"
                          html $ mconcat [T.pack (show (B.unconfirmedTransactions bChain) ++ "\n")]

