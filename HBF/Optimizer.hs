module HBF.Optimizer
( optimize
) where

import HBF.Types (Cmds, Cmd(..))


optimize :: Cmds -> Cmds
optimize = optimizeInc


optimizeInc :: Cmds -> Cmds
optimizeInc = incMerge . map (incExpand . incLoop)

incLoop :: Cmd -> Cmd
incLoop (Loop l) = Loop $ optimizeInc l
incLoop x = x

incExpand :: Cmd -> Cmd
incExpand IncVal = IncValBy 1
incExpand DecVal = DecValBy 1
incExpand x = x

incMerge :: Cmds -> Cmds
incMerge [] = []
incMerge (IncValBy n : IncValBy m : xs) = incMerge $ IncValBy (n+m) : xs
incMerge (DecValBy n : DecValBy m : xs) = incMerge $ DecValBy (n+m) : xs
incMerge (x : xs) = x : incMerge xs
