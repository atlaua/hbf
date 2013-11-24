module HBF.Tape
( Tape(..)
, decCurVal
, incCurVal
, TapeState
, Val
) where

import HBF.Types (TapeState, Val)


class (Functor t, Monad t) => Tape t where
    -- Core functions
    moveLeft :: t ()
    moveRight :: t ()

    readCurVal :: t Val
    writeCurVal :: Val -> t ()

    -- Additional functions
    modifyCurVal :: (Val -> Val) -> t ()
    modifyCurVal f = fmap f readCurVal >>= writeCurVal

decCurVal :: Tape t => t ()
decCurVal = modifyCurVal (\x -> x-1)

incCurVal :: Tape t => t ()
incCurVal = modifyCurVal (+1)
