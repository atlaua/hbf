{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, NamedFieldPuns, RecordWildCards #-}

module HBF.Tape.CrumbList
( CrumbList
, runCrumbList
) where

import Control.Applicative
import Control.Monad.State

import HBF.Tape


newtype CrumbList a = CrumbList {getCrumbList :: State (CL LCR) a} deriving (Functor, Monad)

runCrumbList :: CrumbList a -> (Pos, [Val])
runCrumbList cl = getCL $ execState (getCrumbList cl) emptyCL

instance Tape CrumbList where
    readCurVal = CrumbList $ fmap cur get
    writeCurVal v = CrumbList $ modify (\cl -> cl {cur = v})
    modifyCurVal f = CrumbList $ modify (\cl@CL {cur} -> cl {cur = f cur})

    moveLeft = CrumbList $ modify shiftCL
    moveRight = CrumbList $ modify revShiftCL


data LCR
data RCL

data CL a = CL
    { left  :: [Val]
    , cur   :: Val
    , right :: [Val]
    }

emptyCL :: CL a
emptyCL = CL { left  = []
             , cur   = 0
             , right = []
             }

getCL :: CL LCR -> (Pos, [Val])
getCL CL {..} = (length left, reverse left ++ [cur] ++ right)

shiftCL :: CL a -> CL a
shiftCL CL {..} = CL {left=tleft, cur=hleft, right=cur:right}
    where (hleft:tleft) = expand left

revShiftCL :: CL LCR -> CL LCR
revShiftCL = rcl2lcr . shiftCL . lcr2rcl

lcr2rcl :: CL LCR -> CL RCL
lcr2rcl CL {..} = CL {left = right, cur = cur, right = left}

rcl2lcr :: CL RCL -> CL LCR
rcl2lcr CL {..} = CL {left = right, cur = cur, right = left}

expand :: [Val] -> [Val]
expand [] = [0]
expand a = a
