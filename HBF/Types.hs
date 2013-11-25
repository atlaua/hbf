module HBF.Types
( Cmds
, Cmd(..)
, Val
, TapeState
) where

import Data.Word


type Cmds = [Cmd]

data Cmd = MoveLeft
         | MoveRight
         | WriteVal
         | ReadVal
         | Loop Cmds
         -- ^ core
         | IncVal
         | DecVal
         -- ^ raw
         | IncValBy Val
         | DecValBy Val
         -- ^ optimized
         deriving Show


type Val = Word
type TapeState = ([Val], Val, [Val])
