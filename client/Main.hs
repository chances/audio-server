{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit ((.|), runConduitRes)
import qualified Data.Conduit.Audio as Audio
import Data.Conduit.Network (runTCPClient, clientSettings, appSource, appSink)

import Sound.Server.Pulse (sinkSimple, decodeBytes)

main :: IO ()
main = do
  putStrLn "Audio Server - Client"
  putStrLn ""

  putStrLn "Starting client on 127.0.0.1:4000"

  runTCPClient (clientSettings 4000 "127.0.0.1") $ \server ->
    runConduitRes
      $ appSource server
     .| decodeBytes
     .| sinkSimple
