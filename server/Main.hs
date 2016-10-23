{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

-- import System.Environment (getArgs)
-- import System.Exit (exitFailure)
-- import System.IO (hPutStrLn, stderr)
import Control.Concurrent (threadDelay)
import Data.Conduit (runConduit)
import qualified Data.Conduit.Combinators as Comb
import qualified Data.Conduit.Audio as Audio
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

  putStrLn "Recording for 8 seconds..."
  sample <- record device 8
  putStrLn "Done"

  putStr "Length of sample: "
  print (length sample)

  threadDelay 1500000

  putStrLn "Playing back sample"
  play sample
