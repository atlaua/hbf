{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HBF.PrgmIO.Direct
( IOFormat(..)
, DirectPrgmIO
, runDirectPrgmIO
) where

import Control.Applicative
import Control.Monad.Reader
import Data.Char

import HBF.PrgmIO


data IOFormat = CharFormat | IntFormat

readFunc :: IOFormat -> IO Val
readFunc CharFormat = putStr "< " >> fmap ord getChar <* putStrLn ""
readFunc IntFormat = putStr "< " >> fmap read getLine

writeFunc :: Val -> IOFormat -> IO ()
writeFunc x CharFormat = putChar (chr x)
writeFunc x IntFormat = putStr "> " >> print x


newtype DirectPrgmIO a = DirectPrgmIO {getDirectPrgmIO :: ReaderT IOFormat IO a} deriving (Functor, Monad)

runDirectPrgmIO :: IOFormat -> DirectPrgmIO a -> IO a
runDirectPrgmIO f = flip runReaderT f . getDirectPrgmIO

instance PrgmIO DirectPrgmIO where
    prgmRead = DirectPrgmIO $ asks readFunc >>= liftIO
    prgmWrite x = DirectPrgmIO $ asks (writeFunc x) >>= liftIO
