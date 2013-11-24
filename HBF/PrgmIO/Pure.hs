{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HBF.PrgmIO.Pure
( PurePrgmIO
, runPurePrgmIO
, Val
) where

import Control.Monad.State
import Control.Monad.Writer

import HBF.PrgmIO


newtype PurePrgmIO a = PurePrgmIO {getPurePrgmIO :: WriterT [Val] (State [Val]) a} deriving (Functor, Monad)

runPurePrgmIO :: [Val] -> PurePrgmIO a -> [Val]
runPurePrgmIO input = flip evalState input . execWriterT . getPurePrgmIO

instance PrgmIO PurePrgmIO where
    prgmRead = PurePrgmIO $ state (\(x:xs) -> (x, xs))
    prgmWrite x = PurePrgmIO $ tell [x]
