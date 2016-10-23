module Main where

import Control.Concurrent (threadDelay)
import Data.Maybe (fromJust)

import Sound.Server.Pulse (sinkSimple)

main :: IO ()
main = do
  putStrLn "Audio Server - Client"
