{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HBF.PrgmIO.Direct
( CharPrgmIO
, runCharPrgmIO
, IntPrgmIO
, runIntPrgmIO
) where

import Control.Applicative
import Data.Char

import HBF.PrgmIO


newtype CharPrgmIO a = CharPrgmIO {runCharPrgmIO :: IO a} deriving (Functor, Monad)

instance PrgmIO CharPrgmIO where
    prgmRead = CharPrgmIO $ putStr "< " *> fmap ord getChar <* putStrLn ""
    prgmWrite = CharPrgmIO . putChar . chr


newtype IntPrgmIO a = IntPrgmIO {runIntPrgmIO :: IO a} deriving (Functor, Monad)

instance PrgmIO IntPrgmIO where
    prgmRead = IntPrgmIO $ putStr "< " >> fmap read getLine
    prgmWrite x = IntPrgmIO . putStrLn $ "> " ++ show x
