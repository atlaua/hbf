module HBF.Tape
( Tape(..)
, incCurValBy
, decCurValBy
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

incCurValBy :: Tape t => Val -> t ()
incCurValBy v = modifyCurVal (+v)

decCurValBy :: Tape t => Val -> t ()
decCurValBy v = modifyCurVal (\x -> x-v)
