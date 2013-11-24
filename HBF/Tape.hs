module HBF.Tape
( Tape(..)
, incCurValBy
, decCurValBy
, Offset
, TapeState
, Val
) where

import Control.Monad
import Data.List

import HBF.Types (Offset, TapeState, Val)


class (Functor t, Monad t) => Tape t where
    -- Core functions
    moveLeft :: t ()
    moveRight :: t ()

    readCurVal :: t Val
    writeCurVal :: Val -> t ()

    -- Additional functions
    modifyCurVal :: (Val -> Val) -> t ()
    modifyCurVal f = fmap f readCurVal >>= writeCurVal

    moveRightBy :: Offset -> t ()
    moveRightBy n | n > 0 = replicateM_ n moveRight
                  | n < 0 = replicateM_ (-n) moveLeft
                  | n == 0 = return ()

    flatLoop :: [Offset] -> t ()
    flatLoop xs = readCurVal >>= flatLoop' xs

flatLoop' :: Tape t => [Offset] -> Val -> t ()
flatLoop' xs v = sequence_ ops >> retractAndZero
    where (finalPos, ops) = mapAccumL moveAndWrite 0 xs
          retractAndZero = moveRightBy (-finalPos) >> writeCurVal 0
          moveAndWrite curPos targetPos = (targetPos, moveRightBy (targetPos-curPos) >> incCurValBy v)


incCurValBy :: Tape t => Val -> t ()
incCurValBy v = modifyCurVal (+v)

decCurValBy :: Tape t => Val -> t ()
decCurValBy v = modifyCurVal (\x -> x-v)
