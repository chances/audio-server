{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import System.Environment (getArgs)
-- import System.Exit (exitFailure)
-- import System.IO (hPutStrLn, stderr)
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit ((.|), runConduitRes)
import qualified Data.Conduit.Audio as Audio
import Data.Conduit.Network (runTCPServer, serverSettings, appSource, appSink)
import Data.Maybe (fromJust)
import Data.Vector.Storable (toList)

import Sound.Server.Pulse (sourceSimpleFromDevice, encodeBytes)
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

  runTCPServer (serverSettings 4000 "127.0.0.1") $ \appData -> do
    audioSource <- sourceSimpleFromDevice device
    runConduitRes
      $ Audio.source audioSource
     .| encodeBytes
     .| appSink appData
