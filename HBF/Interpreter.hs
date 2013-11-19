module HBF.Interpreter
( runCmds
) where

import Control.Monad

import HBF.PrgmIO
import HBF.Tape
import HBF.Types


runCmds :: (PrgmIO m, Tape m) => Cmds -> m ()
runCmds = mapM_ runCmd

runCmd :: (PrgmIO m, Tape m) => Cmd -> m ()
runCmd MoveLeft = moveLeft
runCmd MoveRight = moveRight
runCmd IncVal = incCurVal
runCmd DecVal = decCurVal
runCmd WriteVal = readCurVal >>= prgmWrite
runCmd ReadVal = prgmRead >>= writeCurVal
runCmd (Loop cmds) = runLoop cmds

runLoop :: (PrgmIO m, Tape m) => Cmds -> m ()
runLoop cmds = do
    cur <- readCurVal
    when (cur > 0) $ runCmds cmds >> runLoop cmds
