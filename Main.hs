{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{-# OPTIONS_GHC -Wall          #-}

module Main where

import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Trans.Class    (MonadTrans (lift))
import           Data.ByteString              (ByteString)
import           Streaming.Prelude            (Stream)
--import qualified Data.Attoparsec.ByteString.Char8     as A
import           Data.Functor.Of
import qualified Streaming.Prelude            as S
import qualified System.IO.TailFile.Streaming as T


-- 1.6 million lines, wc -l takes 586ms
-- filetoread = "/home/shae/download/incoming/crashdata/nyc.crash.json"
filetoread :: FilePath
filetoread = "/tmp/ixess.log"

main :: IO ()
main = do
  print "hi"
  something filetoread

callback :: forall t r void. (MonadTrans t, MonadIO (t IO)) => Stream (Of ByteString) (t IO) r -> t IO (Of void r)
callback s = do
  let go st = S.next st >>= \case
        Left _ -> error "impossible: end of stream"
        Right (x,next) -> do
          lift $ print x
          go next
  go s

something :: FilePath -> IO void
something fp = T.tailFile fp callback
