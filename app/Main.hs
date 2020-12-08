module Main where

import Lib
import qualified EchoServer as S

main :: IO ()
main = S.runServer
