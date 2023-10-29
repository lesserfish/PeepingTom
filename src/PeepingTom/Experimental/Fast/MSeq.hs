module PeepingTom.Experimental.Fast.MSeq where

import qualified Data.Foldable as F
import Data.IORef
import Data.Sequence (Seq, empty, (|>))

type MSeq a = IORef (Seq a)

makeMSeq :: IO (MSeq a)
makeMSeq = newIORef empty

push :: MSeq a -> a -> IO ()
push seqRef element = do
    mseq <- readIORef seqRef
    writeIORef seqRef (mseq |> element)

toList :: MSeq a -> IO [a]
toList seqRef = do
    mseq <- readIORef seqRef
    return $ F.toList mseq
