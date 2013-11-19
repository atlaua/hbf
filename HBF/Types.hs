module HBF.Types
( Cmds
, Cmd(..)
, Val
, TapeState
) where


type Cmds = [Cmd]

data Cmd = MoveLeft
         | MoveRight
         | IncVal
         | DecVal
         | WriteVal
         | ReadVal
         | Loop Cmds
         deriving (Eq, Show)


type Val = Int
type TapeState = ([Val], Val, [Val])
