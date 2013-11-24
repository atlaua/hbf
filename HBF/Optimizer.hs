module HBF.Optimizer
( optimize
) where

import Data.Maybe

import HBF.Types


optimize :: Cmds -> Cmds
optimize = optimizeInc


optimizeInc :: Cmds -> Cmds
optimizeInc = mapMaybe incReduce . incMerge . map (incExpand . incLoop)

incLoop :: Cmd -> Cmd
incLoop (Loop l) = Loop $ optimizeInc l
incLoop x = x

incExpand :: Cmd -> Cmd
incExpand IncVal = IncValBy 1
incExpand DecVal = IncValBy (-1)
incExpand x = x

incMerge :: Cmds -> Cmds
incMerge [] = []
incMerge (IncValBy n : IncValBy m : xs) = incMerge $ IncValBy (n+m) : xs
incMerge (x : xs) = x : incMerge xs

incReduce :: Cmd -> Maybe Cmd
incReduce (IncValBy 0   ) = Nothing
incReduce (IncValBy 1   ) = Just IncVal
incReduce (IncValBy (-1)) = Just DecVal
incReduce x = Just x
