module Main where

import Criterion.Main

import HBF.Interpreter
import HBF.Parser
import HBF.PrgmIO.Pure
import HBF.Tape.CrumbList


main :: IO ()
main = defaultMain [ bgroup "fast" [bench "square_100" $ whnf (runBF squareBF) [100]]
                   , bgroup "slow" [bench "square_500" $ whnf (runBF squareBF) [500]]
                   ]

runBF :: String -> [Val] -> [Val]
runBF bf input = runPurePrgmIO input  . runCrumbListT . runCmds . getRight $ parseBF bf
    where getRight (Right x) = x

squareBF :: String
squareBF = ",[->+>+<<]>[->[->+>+<<]>>[-<<+>>]<[->>+<<]<<]>>>>."
