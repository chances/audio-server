{-# LANGUAGE OverloadedStrings #-}

module Sound.Server.Devices
    ( getMonitorDeviceNames
    , promptForDevice
    ) where

import Data.Char (digitToInt)
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.Text
import qualified System.Command as C
import System.Process (readCreateProcessWithExitCode)

promptForDevice :: IO (Maybe String)
promptForDevice = do
  devices <- getMonitorDeviceNames
  putStrLn "Choose a device:"
  printDevices devices
  chosenDeviceIndex <- getLine
  let index = digitToInt $ Prelude.head chosenDeviceIndex in
    return $ devices `elemAt` index

printDevices :: [String] -> IO ()
printDevices devices = mapM_
  (\device ->
    putStrLn (show (fromJust (elemIndex device devices)) ++ ": " ++ device))
  devices

elemAt :: [a] -> Int -> Maybe a
elemAt [] _ = Nothing
elemAt (x:_) 0 = Just x
elemAt (_:xs) index = xs `elemAt` (index - 1)

getMonitorDeviceNames :: IO [String]
getMonitorDeviceNames =
  let cmd = C.shell "pacmd list-sources | grep -e 'stereo.monitor'" in do
    (exitcode, stdout, stderr) <- readCreateProcessWithExitCode cmd ""
    let
      deviceNames = Prelude.map parseDeviceName $ Data.Text.lines $ pack stdout
      in return $ Prelude.map unpack deviceNames

parseDeviceName :: Text -> Text
parseDeviceName input = stripFirstLast $ Prelude.last $ splitAndStripLine input

splitAndStripLine :: Text -> [Text]
splitAndStripLine line = splitOn (pack " ") (strip line)

stripFirstLast :: Text -> Text
stripFirstLast str = pack $ firstLast $ unpack (strip str)

firstLast :: [a] -> [a]
firstLast [] = []
firstLast [x] = []
firstLast xs = Prelude.tail (Prelude.init xs)
