{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Conduit ((.|), runConduitRes)
import Data.Conduit.Network (runTCPServer, serverSettings, appSource, appSink)
import Data.Maybe (fromJust)

import Sound.Server.Pulse (sourceSimpleFromDevice)
import Sound.Server.Devices (promptForDevice)

-- main :: IO ()
-- main = do args <- getArgs
--           case args of
--             [file] -> record file
--             _      -> do hPutStrLn stderr "Usage: record <file.pcm>"
--                          exitFailure

main :: IO ()
main = do
  device <- promptForDevice
  putStr "Using device: "
  putStrLn (fromJust device)

  putStrLn "Starting server on 127.0.0.1:4000"

  runTCPServer (serverSettings 4000 "127.0.0.1") $ \appData ->
    runConduitRes
      $ sourceSimpleFromDevice device
     .| appSink appData
