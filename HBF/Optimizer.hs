module HBF.Optimizer
( optimize
) where

import HBF.Types


-----
optimize :: Cmds -> Cmds

optimize [] = []

optimize (IncVal     : IncVal : xs) = optimize $ IncValBy 2     : xs
optimize (DecVal     : DecVal : xs) = optimize $ IncValBy (-2)  : xs

optimize (IncVal     : DecVal : xs) = optimize xs
optimize (DecVal     : IncVal : xs) = optimize xs

optimize (IncValBy n : IncVal : xs) = optimize $ IncValBy (n+1) : xs
optimize (IncValBy n : DecVal : xs) = optimize $ IncValBy (n-1) : xs

optimize (IncValBy 0    : xs) = optimize xs
optimize (IncValBy 1    : xs) = IncVal : optimize xs
optimize (IncValBy (-1) : xs) = DecVal : optimize xs

optimize (x : xs) = x : optimize xs
-----
