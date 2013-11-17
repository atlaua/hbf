module HBF.Tape
( Pos
, Val
, Tape(..)
, decCurVal
, incCurVal
) where

import HBF.Types (Val)

type Pos = Int

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
decCurVal = modifyCurVal pred

incCurVal :: Tape t => t ()
incCurVal = modifyCurVal succ
