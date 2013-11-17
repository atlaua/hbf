{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns, RecordWildCards #-}

module HBF.Tape.CrumbList
( CrumbList
, runCrumbList
) where

import Control.Applicative
import Control.Monad.State

import HBF.Tape

data CL = CL
    { left  :: [Val]
    , cur   :: Val
    , right :: [Val]
    }

emptyCL :: CL
emptyCL = CL { left  = []
             , cur   = 0
             , right = []
             }

getCL :: CL -> [Val]
getCL CL {..} = reverse left ++ [cur] ++ right

newtype CrumbList a = CrumbList {getCrumbList :: State CL a} deriving (Functor, Monad)

runCrumbList :: CrumbList a -> [Val]
runCrumbList cl = getCL $ execState (getCrumbList cl) emptyCL

instance Tape CrumbList where
    readCurVal = CrumbList $ fmap cur get
    writeCurVal v = CrumbList $ modify (\cl -> cl {cur = v})
    modifyCurVal f = CrumbList $ modify (\cl@CL {cur} -> cl {cur = f cur})

    moveLeft = CrumbList $ modify shiftCL
    moveRight = CrumbList $ modify revShiftCL

shiftCL :: CL -> CL
shiftCL CL {..} = CL {left=tleft, cur=hleft, right=cur:right}
    where (hleft:tleft) = expand left

revShiftCL :: CL -> CL
revShiftCL = revCL . shiftCL . revCL

-- Always use this in pairs, otherwise hell breaks loose ;)
revCL :: CL -> CL
revCL CL {..} = CL {left = right, cur = cur, right = left}

expand :: [Val] -> [Val]
expand [] = [0]
expand a = a
