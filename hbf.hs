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
    cmds <- either exitParseError return . parseBF =<< getCmds file cmdStr

    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    tape <- runDirectPrgmIO (translate ioType) . runCrumbListT $ runCmds cmds
    putStrLn $ "\nFinal Tape:\n" ++ show tape

getCmds :: String -> String -> IO String
getCmds "" cmds = return cmds
getCmds file _ = readFile file

exitParseError :: ParseError -> IO a
exitParseError e = putStrLn "Parse error:" >> print e >> exitFailure


data IOType = CharIO | IntIO deriving (Data, Typeable)
data HBF = HBF {ioType :: IOType, cmdStr :: String, file :: String} deriving (Data, Typeable)

translate :: IOType -> IOFormat
translate CharIO = CharFormat
translate IntIO = IntFormat

hbf = cmdArgsMode $ HBF { ioType = enum [IntIO &= help "Use Int IO (default)", CharIO &= help "Use Char IO"]
                        , cmdStr = def &= args &= typ "Brainfuck Code"
                        , file = def &= typFile &= help "File to read Brainfuck code from"
                        } &= summary "hbf v0.1 - Experimental Haskell Brainfuck interpreter"
