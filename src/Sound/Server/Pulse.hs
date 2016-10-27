module Sound.Server.Pulse
    ( record, play
    , sourceSimpleFromDevice
    , sinkSimple
    , encodeBytes
    , decodeBytes
    ) where

import qualified Sound.Pulse.Simple as P
import Data.ByteString (ByteString)
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as Comb
import Data.Conduit.Audio (AudioSource(..), Channels)
import Data.Serialize.Put (runPut)
import Data.Serialize.Get (runGet)
import Data.Serialize.IEEE754 (putFloat32le, getFloat32le)
import Data.Vector.Storable (Vector(..), singleton, fromList, toList)
import Data.Void (Void(..))
import Control.Applicative (many)
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
    samples <- liftIO $ P.simpleRead simple 32
    C.yield $ fromList samples
    loop

encodeBytes :: (MonadResource m) => C.ConduitM (Vector Float) ByteString m ()
encodeBytes = Comb.map encodeToBytes

encodeToBytes :: Vector Float -> ByteString
encodeToBytes = runPut . mapM_ putFloat32le . toList

play :: [Float] -> IO ()
play d = do
    s <- P.simpleNew Nothing "audio-server" P.Play Nothing "Stream audio to hosts across your LAN"
        (P.SampleSpec (P.F32 P.LittleEndian) 44100 1) Nothing Nothing
    P.simpleWrite s d
    P.simpleDrain s
    P.simpleFree s

sinkSimple :: (MonadResource m) => C.ConduitM (Vector Float) Void m ()
sinkSimple =
  let r = 44100 :: Int
      c = 2
  in C.bracketP
    (P.simpleNew Nothing "audio-server-client" P.Play Nothing "Stream audio to hosts across your LAN"
        (P.SampleSpec (P.F32 P.LittleEndian) r c) Nothing Nothing)
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

decodeBytes :: (MonadResource m) => C.ConduitM ByteString (Vector Float) m ()
decodeBytes = Comb.map decodeToVector

decodeToVector :: ByteString -> Vector Float
decodeToVector sample = fromList sampleList where
  sampleList = case bytesToFloats sample of
    Left x -> [0.0 :: Float]
    Right x -> x

bytesToFloats :: ByteString -> Either String [Float]
bytesToFloats = runGet $ many getFloat32le
