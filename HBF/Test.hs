module HBF.Test where

import HBF.Interpreter
import HBF.Optimizer
import HBF.Parser
import HBF.PrgmIO.Pure
import HBF.Tape.CrumbList


runTestBF :: Bool -> [Val] -> String -> [Val]
runTestBF opt input bf = runPurePrgmIO input . runCrumbListT . runCmds . optimizeIf opt . getRight $ parseBF bf
    where getRight (Right x) = x

bfAdd = ",>,[-<+>]<."
bfMul = ",>,<[->[->+>+<<]>[-<+>]<<]>>>."
bfSqu = ",[->+>+<<]>[->[->+>+<<]>>[-<<+>>]<[->>+<<]<<]>>>>."
