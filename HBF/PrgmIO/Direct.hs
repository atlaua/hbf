{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HBF.PrgmIO.Direct
( CharPrgmIO
, runCharPrgmIO
, IntPrgmIO
, runIntPrgmIO
) where

import Control.Monad.State
import Data.Char

import HBF.PrgmIO


data CharIOMode = Init | Read | Write deriving Eq

newtype CharPrgmIO a = CharPrgmIO {getCharPrgmIO :: StateT CharIOMode IO a} deriving (Functor, Monad)

runCharPrgmIO :: CharPrgmIO a -> IO a
runCharPrgmIO = flip evalStateT Init . getCharPrgmIO

instance PrgmIO CharPrgmIO where
    prgmRead = CharPrgmIO $ modePrompt Read "\n< " >> fmap (fromIntegral . ord) (liftIO getChar)
    prgmWrite x = CharPrgmIO $ modePrompt Write "\n> " >> liftIO (putChar . chr $ fromIntegral x)

modePrompt :: CharIOMode -> String -> StateT CharIOMode IO ()
modePrompt newMode prompt = do
    curMode <- get
    when (curMode /= newMode) $ liftIO (putStr prompt)
    put newMode


newtype IntPrgmIO a = IntPrgmIO {runIntPrgmIO :: IO a} deriving (Functor, Monad)

instance PrgmIO IntPrgmIO where
    prgmRead = IntPrgmIO $ putStr "< " >> fmap read getLine
    prgmWrite x = IntPrgmIO . putStrLn $ "> " ++ show x
