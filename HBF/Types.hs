module HBF.Types
( Val
, Cmds
, Cmd(..)
) where

type Val = Int
type Cmds = [Cmd]

data Cmd = MoveLeft
         | MoveRight
         | IncVal
         | DecVal
         | WriteVal
         | ReadVal
         | Loop Cmds
         deriving (Eq, Show)
