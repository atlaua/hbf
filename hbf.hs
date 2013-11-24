{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main where

import Control.Monad
import Text.Parsec.Error
import System.Console.CmdArgs
import System.Exit
import System.IO

import HBF.Interpreter
import HBF.Parser
import HBF.PrgmIO.Direct
import HBF.Tape.CrumbList


main :: IO ()
main = do
    HBF {..} <- cmdArgsRun hbf
    cmds <- either exitParseError return . parseBF =<< getCmds file cmdStr

    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    tape <- case ioType of
        CharIO -> runCharPrgmIO . runCrumbListT $ runCmds cmds
        IntIO -> runIntPrgmIO . runCrumbListT $ runCmds cmds

    unless noFinalTape $ putStrLn $ "\nFinal Tape:\n" ++ show tape

getCmds :: String -> String -> IO String
getCmds "" cmds = return cmds
getCmds file _ = readFile file

exitParseError :: ParseError -> IO a
exitParseError e = putStrLn "Parse error:" >> print e >> exitFailure


data IOType = CharIO | IntIO deriving (Data, Typeable)
data HBF = HBF {cmdStr :: String, file :: String, ioType :: IOType, noFinalTape :: Bool} deriving (Data, Typeable)

hbf = cmdArgsMode $ HBF { cmdStr = def &= args &= typ "Brainfuck Code"
                        , file = def &= typFile &= help "File to read Brainfuck code from"
                        , ioType = enum [IntIO &= help "Use Int IO (default)", CharIO &= help "Use Char IO"]
                        , noFinalTape = False &= help "Don't show final tape state"
                        } &= summary "hbf v0.3 - Experimental Haskell Brainfuck interpreter"
