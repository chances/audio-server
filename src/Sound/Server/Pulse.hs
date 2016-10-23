module Sound.Server.Pulse
    ( record, play
    , sourceSimpleFromDevice
    , sinkSimple
    ) where

import qualified Sound.Pulse.Simple as P
import Data.ByteString (ByteString)
import qualified Data.Conduit as C
import Data.Conduit.Audio (AudioSource(..), Channels)
import Data.Serialize.Put (runPut)
import Data.Serialize.IEEE754 (putFloat32le)
import Data.Vector.Storable (Vector(..), singleton, toList)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Fix (fix)
import Control.Monad.Trans.Resource (MonadResource)

record :: Maybe String -> Int -> IO [Float]
record device durationSeconds = do
  s <- P.simpleNew Nothing "audio-server" P.Record device "Stream audio to hosts across your LAN"
      (P.SampleSpec (P.F32 P.LittleEndian) 44100 1) Nothing Nothing
  xs <- P.simpleRead s $ 44100 * durationSeconds :: IO [Float]
  P.simpleFree s
  return xs

sourceSimpleFromDevice :: (MonadResource m) => Maybe String -> IO (AudioSource m Float)
sourceSimpleFromDevice device = do
  let r = 44100 :: Int
      c = 2
      source = C.bracketP
        (P.simpleNew Nothing "audio-server" P.Record device "Stream audio to hosts across your LAN"
            (P.SampleSpec (P.F32 P.LittleEndian) r c) Nothing Nothing)
        P.simpleFree
        sourceSimple
  return $ AudioSource source (fromIntegral r) c 2

sourceSimple :: (MonadResource m) => P.Simple -> C.Source m (Vector Float)
sourceSimple simple = loop where
  loop = do
    samples <- liftIO $ P.simpleRead simple 1
    C.yield $ singleton $ head samples
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

sinkSimple :: (MonadResource m) => AudioSource m Float -> m ()
sinkSimple (AudioSource s r c _) = (C.$$) s $
  C.bracketP
    (P.simpleNew Nothing "audio-server" P.Play Nothing "Stream audio to hosts across your LAN"
        (P.SampleSpec (P.F32 P.LittleEndian) (truncate r) c) Nothing Nothing)
    P.simpleFree
    sinkSimpleHandle

-- sinkSimpleWithHandle :: (MonadResource m) => P.Simple -> AudioSource m a -> m ()
-- sinkSimpleWithHandle simple (AudioSource s r c _) = do
--   let r = 44100 :: Int
--       c = 2
--   C.bracketP
--     (P.simpleNew Nothing "audio-server" P.Play Nothing "Stream audio to hosts across your LAN"
--         (P.SampleSpec (P.F32 P.LittleEndian) r c) Nothing Nothing)
--     P.simpleFree
--     sinkSimpleHandle

sinkSimpleHandle :: (MonadResource m) => P.Simple -> C.Sink (Vector Float) m ()
sinkSimpleHandle simple = C.awaitForever $ liftIO . write where
  write sample = do
    P.simpleWrite simple $ toList sample
    P.simpleDrain simple
