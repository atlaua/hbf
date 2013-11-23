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
         | IncVal
         | DecVal
         | WriteVal
         | ReadVal
         | Loop Cmds
         -- ^ core, v optimized
         | IncValBy Val
         deriving (Eq, Show)


type Val = Word
type TapeState = ([Val], Val, [Val])
