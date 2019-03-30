{-# LANGUAGE OverloadedStrings #-}
-- working version of the code in this stack overflow post:
-- https://stackoverflow.com/questions/41230293/how-to-efficiently-follow-tail-a-file-with-haskell-including-detecting-file

module Main where

import           Control.Concurrent.QSem
import qualified Data.ByteString
import qualified Data.ByteString.Char8         as BC8
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified Data.ByteString.Streaming     as B
import           Streaming
import           System.Environment            (getArgs)
import           System.INotify
import           System.IO                     (IOMode (ReadMode), withFile)

tailing :: FilePath -> (B.ByteString IO () -> IO r) -> IO r
tailing filepath continuation = withINotify $ \i -> do
    sem <- newQSem 1
    _ <- addWatch i [Modify, CloseWrite] (BC8.pack filepath) (\_ -> signalQSem sem)
    withFile filepath ReadMode (\h -> continuation (handleToStream sem h))
    where
    handleToStream sem h = B.concat . Streaming.repeats $ do
        lift (waitQSem sem)
        readWithoutClosing h
    -- Can't use B.fromHandle here because annoyingly it closes handle on EOF
    -- instead of just returning, and this causes problems on new appends.
    readWithoutClosing h = do
        c <- lift (Data.ByteString.hGetSome h defaultChunkSize)
        if Data.ByteString.null c
           then return ()
           else do B.chunk c
                   readWithoutClosing h

main :: IO ()
main = do
  filepath : _ <- getArgs
  tailing filepath B.stdout

