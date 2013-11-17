module HBF.Interpreter
( runCmds
) where

import Control.Monad

import HBF.Tape
import HBF.Types


runCmds :: (Tape t) => Cmds -> t ()
runCmds = mapM_ runCmd

runCmd :: (Tape t) => Cmd -> t ()
runCmd MoveLeft = moveLeft
runCmd MoveRight = moveRight
runCmd IncVal = incCurVal
runCmd DecVal = decCurVal
runCmd (Loop cmds) = runLoop cmds

runLoop :: (Tape t) => Cmds -> t ()
runLoop cmds = do
    cur <- readCurVal
    when (cur > 0) $ runCmds cmds >> runLoop cmds
