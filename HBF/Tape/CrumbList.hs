{-# LANGUAGE FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, NamedFieldPuns, RecordWildCards #-}

module HBF.Tape.CrumbList
( CrumbListT
, runCrumbListT
) where

import Control.Applicative
import Control.Monad.State

import HBF.PrgmIO
import HBF.Tape
import HBF.Types (TapeState, Val)


newtype CrumbListT m a = CrumbListT {getCrumbListT :: StateT (CL LCR) m a} deriving (Functor, Monad, MonadTrans)

runCrumbListT :: (Functor m, Monad m) => CrumbListT m a -> m TapeState
runCrumbListT cl = getCL <$> execStateT (getCrumbListT cl) emptyCL

instance (Functor m, Monad m) => Tape (CrumbListT m) where
    readCurVal = CrumbListT $ cur <$> get
    writeCurVal v = CrumbListT $ modify (\cl -> cl {cur = v})
    modifyCurVal f = CrumbListT $ modify (\cl@CL {cur} -> cl {cur = f cur})

    moveLeft = CrumbListT $ modify shiftCL
    moveRight = CrumbListT $ modify revShiftCL

instance (PrgmIO m) => PrgmIO (CrumbListT m) where
    prgmRead = lift prgmRead
    prgmWrite = lift . prgmWrite


data LCR
data RCL

class Rev a b | a -> b
instance Rev LCR RCL
instance Rev RCL LCR


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

getCL :: CL LCR -> TapeState
getCL CL {..} = (length left, reverse left ++ [cur] ++ right)

shiftCL :: CL a -> CL a
shiftCL CL {..} = CL {left=tleft, cur=hleft, right=cur:right}
    where (hleft:tleft) = expand left

revShiftCL :: CL LCR -> CL LCR
revShiftCL = revCL . shiftCL . revCL

revCL :: (Rev a b) => CL a -> CL b
revCL CL {..} = CL {left = right, cur = cur, right = left}

expand :: [Val] -> [Val]
expand [] = [0]
expand a = a
