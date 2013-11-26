{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main where

import Control.Monad
import Text.Parsec.Error
import System.Console.CmdArgs
import System.Exit
import System.IO

import HBF.Interpreter
import HBF.Optimizer
import HBF.Parser
import HBF.PrgmIO.Direct
import HBF.Tape.CrumbList
import HBF.Types (Cmds)


main :: IO ()
main = do
    HBF {..} <- cmdArgsRun hbf
    cmds <- fmap (optimizeIf $ not raw) . exitOnParseError . parseBF =<< getCmds file cmdStr

    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    tape <- case ioType of
        CharIO -> runCharPrgmIO . runCrumbListT $ runCmds cmds
        IntIO -> runIntPrgmIO . runCrumbListT $ runCmds cmds

    unless noFinalTape $ putStrLn $ "\nFinal Tape:\n" ++ show tape

getCmds :: String -> String -> IO String
getCmds "" cmds = return cmds
getCmds file _ = readFile file

exitOnParseError :: Either ParseError Cmds -> IO Cmds
exitOnParseError (Left e) = putStrLn "Parse error:" >> print e >> exitFailure
exitOnParseError (Right c) = return c


data IOType = CharIO | IntIO deriving (Data, Typeable)
data HBF = HBF { cmdStr      :: String
               , file        :: String
               , noFinalTape :: Bool
               , raw         :: Bool
               , ioType      :: IOType
               } deriving (Data, Typeable)

hbf = cmdArgsMode $ HBF { cmdStr      = def &= args &= typ "Brainfuck Code"
                        , file        = def &= typFile &= help "File to read Brainfuck code from"
                        , noFinalTape = def &= help "Don't show final tape state"
                        , raw         = def &= help "Don't optimize code before execution"
                        , ioType      = enum [IntIO &= help "Use Int IO (default)", CharIO &= help "Use Char IO"]
                        } &= summary "hbf v0.3 - Experimental Haskell Brainfuck interpreter"
