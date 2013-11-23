module HBF.PrgmIO
( PrgmIO(..)
, Val
) where

import HBF.Types (Val)


class (Functor s, Monad s) => PrgmIO s where
    prgmRead :: s Val
    prgmWrite :: Val -> s ()
