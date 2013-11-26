module HBF.Types
( Cmds
, Cmd(..)
, Offset
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
         | MoveLoop [Offset]
         -- ^ optimized
         deriving Show

type Offset = Int

type Val = Word
type TapeState = ([Val], Val, [Val])
