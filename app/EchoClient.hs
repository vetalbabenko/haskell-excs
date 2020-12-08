{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module EchoClient where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Lib

client :: IO ()
client = runTCPClient "127.0.0.1" "3000" $ \s -> do loopMessage s


loopMessage :: Socket -> IO ()
loopMessage s = do
    putStrLn "Please, type a message: "
    msg <- getLine
    sendAll s (C.pack msg)
    msg <- recv s 1024
    putStr "Received in CLIENT: "
    C.putStrLn msg
    putStrLn "\n"
    loopMessage s

-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock