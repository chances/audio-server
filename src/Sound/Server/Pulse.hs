module Sound.Server.Pulse
    ( record, play
    , sourceSimpleFromDevice
    , sinkSimple
    ) where

import qualified Sound.Pulse.Simple as P
import Data.ByteString (ByteString)
import qualified Data.Conduit as C
import Data.Serialize.Put (runPut)
import Data.Serialize.IEEE754 (putFloat32le, getFloat32le)
import Data.Vector.Storable (Vector(..), toList)
import Data.Void (Void(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource)

record :: Maybe String -> Int -> IO [Float]
record device durationSeconds = do
  s <- P.simpleNew Nothing "audio-server" P.Record device "Stream audio to hosts across your LAN"
      (P.SampleSpec (P.F32 P.LittleEndian) 44100 1) Nothing Nothing
  xs <- P.simpleRead s $ 44100 * durationSeconds :: IO [Float]
  P.simpleFree s
  return xs

sourceSimpleFromDevice :: (MonadResource m) => Maybe String -> C.ConduitM () ByteString m ()
sourceSimpleFromDevice device =
  let r = 44100 :: Int
      c = 2
  in C.bracketP
        (P.simpleNew Nothing "audio-server" P.Record device "Stream audio to hosts across your LAN"
            (P.SampleSpec (P.F32 P.LittleEndian) r c) Nothing Nothing)
        P.simpleFree
        sourceSimple

sourceSimple :: (MonadResource m) => P.Simple -> C.Source m ByteString
sourceSimple simple = loop where
  loop = do
    samples <- liftIO $ P.simpleRead simple 88200
    C.yield $ encodeToBytes samples
    loop

encodeToBytes :: [Float] -> ByteString
encodeToBytes = runPut . mapM_ putFloat32le

play :: [Float] -> IO ()
play d = do
    s <- P.simpleNew Nothing "audio-server" P.Play Nothing "Stream audio to hosts across your LAN"
        (P.SampleSpec (P.F32 P.LittleEndian) 44100 1) Nothing Nothing
    P.simpleWrite s d
    P.simpleDrain s
    P.simpleFree s

sinkSimple :: (MonadResource m) => C.ConduitM ByteString Void m ()
sinkSimple =
  let r = 44100 :: Int
      c = 2
  in C.bracketP
    (P.simpleNew Nothing "audio-server-client" P.Play Nothing "Stream audio to hosts across your LAN"
        (P.SampleSpec (P.F32 P.LittleEndian) r c) Nothing Nothing)
    P.simpleFree
    sinkSimpleHandle

sinkSimpleHandle :: (MonadResource m) => P.Simple -> C.Sink ByteString m ()
sinkSimpleHandle simple = C.awaitForever $ liftIO . P.simpleWriteRaw simple
