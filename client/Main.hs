{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Conduit ((.|), runConduitRes)
import Data.Conduit.Network (runTCPClient, clientSettings, appSource, appSink)

import Sound.Server.Pulse (sinkSimple)

main :: IO ()
main = do
  putStrLn "Audio Server - Client"
  putStrLn ""

  putStrLn "Starting client on 127.0.0.1:4000"

  runTCPClient (clientSettings 4000 "127.0.0.1") $ \server ->
    runConduitRes
      $ appSource server
     .| sinkSimple
