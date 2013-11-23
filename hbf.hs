{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main where

import Text.Parsec.Error
import System.Console.CmdArgs
import System.Exit
import System.IO

import HBF.Interpreter
import HBF.Parser
import HBF.PrgmIO.Direct
import HBF.Tape.CrumbList
import HBF.Types


main :: IO ()
main = do
    HBF {..} <- cmdArgsRun hbf
    cmds <- either exitParseError return $ parseBF cmdStr

    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    tape <- runDirectPrgmIO (translate ioType) . runCrumbListT $ runCmds cmds
    putStrLn $ "\nFinal Tape:\n" ++ show tape

exitParseError :: ParseError -> IO a
exitParseError e = putStrLn "Parse error:" >> print e >> exitFailure


data IOType = CharIO | IntIO deriving (Data, Typeable)
data HBF = HBF {ioType :: IOType, cmdStr :: String} deriving (Data, Typeable)

translate :: IOType -> IOFormat
translate CharIO = CharFormat
translate IntIO = IntFormat

hbf = cmdArgsMode $ HBF { ioType = enum [IntIO &= help "Use Int IO (default)", CharIO &= help "Use Char IO"]
                        , cmdStr = def &= args
                        } &= summary "hbf - Experimental Haskell Brainfuck interpreter"
