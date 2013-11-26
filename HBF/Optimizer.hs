{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module HBF.Optimizer
( optimize
, optimizeIf
) where

import Control.Monad
import Data.List
import Data.Maybe

import qualified Data.Foldable as F
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import HBF.Types (Cmds, Cmd(..), Offset)


optimizeIf :: Bool -> Cmds -> Cmds
optimizeIf False = id
optimizeIf True = optimize

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


data FlatLoop = FlatLoop {pos :: Offset, factorMap :: IntMap Int}

optimizeLoops :: Cmds -> Cmds
optimizeLoops = map optimizeLoop

optimizeLoop :: Cmd -> Cmd
optimizeLoop (Loop l) = fromMaybe (Loop ol) $ flattenLoop ol
    where ol = optimizeLoops l
optimizeLoop x = x

flattenLoop :: Cmds -> Maybe Cmd
flattenLoop = fmap packFactors . checkLoop <=< flattenLoop'

flattenLoop' :: Cmds -> Maybe FlatLoop
flattenLoop' = foldM go FlatLoop{pos = 0, factorMap = IM.empty}
    where go fl@FlatLoop{pos} MoveLeft  = Just fl{pos = pos-1}
          go fl@FlatLoop{pos} MoveRight = Just fl{pos = pos+1}
          go fl@FlatLoop{..} (IncValBy n) = Just fl{factorMap = IM.insertWith (+) pos (fromIntegral    n) factorMap}
          go fl@FlatLoop{..} (DecValBy n) = Just fl{factorMap = IM.insertWith (+) pos (fromIntegral $ -n) factorMap}
          go _ ReadVal      = Nothing
          go _ WriteVal     = Nothing
          go _ (Loop _)     = Nothing
          go _ (MoveLoop _) = Nothing

checkLoop :: FlatLoop -> Maybe (IntMap Int)
checkLoop FlatLoop{..} | pos == 0 = adjustChanges (IM.delete 0 factorMap) =<< IM.lookup 0 factorMap
                       | otherwise = Nothing

adjustChanges :: IntMap Int -> Int -> Maybe (IntMap Int)
adjustChanges _         0         = Nothing
adjustChanges factorMap (-1)      = Just factorMap
adjustChanges factorMap ctrChange = Just $ fmap (*(-ctrChange)) factorMap

packFactors :: IntMap Int -> Cmd
packFactors factorMap | F.all (==1) factorMap = MoveLoop $ IM.keys factorMap
                      | otherwise = undefined
