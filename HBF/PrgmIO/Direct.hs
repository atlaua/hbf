{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HBF.PrgmIO.Direct
( CharPgrmIO
, runCharPgrmIO
, IntPgrmIO
, runIntPgrmIO
) where

import Control.Applicative
import Data.Char

import HBF.PrgmIO


newtype CharPgrmIO a = CharPgrmIO {runCharPgrmIO :: IO a} deriving (Functor, Monad)

instance PrgmIO CharPgrmIO where
    prgmRead = CharPgrmIO $ putStr "< " *> fmap ord getChar <* putStrLn ""
    prgmWrite x = CharPgrmIO . putStrLn $ "> " ++ [chr x]


newtype IntPgrmIO a = IntPgrmIO {runIntPgrmIO :: IO a} deriving (Functor, Monad)

instance PrgmIO IntPgrmIO where
    prgmRead = IntPgrmIO $ putStr "< " *> fmap read getLine
    prgmWrite x = IntPgrmIO . putStrLn $ "> " ++ show x
