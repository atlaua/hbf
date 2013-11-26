module Main where

import Criterion.Main

import HBF.Test
import HBF.Types (Val)


main :: IO ()
main = defaultMain [ bgroup "opt" [benchSqu True 1000, benchSqu True 5000]
                   , bgroup "raw" [benchSqu False 50, benchSqu False 100]
                   ]

benchSqu :: Bool -> Val -> Benchmark
benchSqu opt n = bench ("squ" ++ show n) $ whnf (runTestBF opt [n]) bfSqu
