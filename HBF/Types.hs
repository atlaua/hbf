module HBF.Types
( Cmds
, Cmd(..)
, Val
, Pos
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
type Pos = Int
type TapeState = (Pos, [Val])
