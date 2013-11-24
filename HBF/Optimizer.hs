module HBF.Optimizer
( optimize
) where

import Data.List

import HBF.Types (Cmds, Cmd(..), Offset)


optimize :: Cmds -> Cmds
optimize = optimizeLoops . optimizeInc


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


optimizeLoops :: Cmds -> Cmds
optimizeLoops = map optimizeLoop

optimizeLoop :: Cmd -> Cmd
optimizeLoop (Loop l) | isPureLoop l = flattenLoop l
                      | otherwise = Loop (optimizeLoops l)
optimizeLoop x = x

isPureLoop :: Cmds -> Bool
isPureLoop = (== Just 0) . fmap sum . mapM moveVal
    where moveVal MoveRight = Just 1
          moveVal MoveLeft = Just (-1)
          moveVal ReadVal = Nothing
          moveVal WriteVal = Nothing
          moveVal (Loop _) = Nothing
          moveVal x = Just 0

flattenLoop :: Cmds -> Cmd
flattenLoop = FlatLoop . snd . foldl' flattenLoop' (0, [])

flattenLoop' :: (Offset, [Offset]) -> Cmd -> (Offset, [Offset])
flattenLoop' (0  , xs) (DecValBy 1) = (0    ,     xs)
flattenLoop' (pos, xs) MoveRight    = (pos+1,     xs)
flattenLoop' (pos, xs) MoveLeft     = (pos-1,     xs)
flattenLoop' (pos, xs) (IncValBy 1) = (pos  , pos:xs)
