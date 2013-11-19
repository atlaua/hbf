{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns, RecordWildCards #-}

module HBF.Tape.CrumbList
( CrumbListT
, runCrumbListT
) where

import Control.Applicative
import Control.Monad.State

import HBF.PrgmIO
import HBF.Tape


newtype CrumbListT m a = CrumbListT {getCrumbListT :: StateT CL m a} deriving (Functor, Monad, MonadTrans)

runCrumbListT :: (Functor m, Monad m) => CrumbListT m a -> m (Pos, [Val])
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

getCL :: CL -> (Pos, [Val])
getCL CL {..} = (length left, reverse left ++ [cur] ++ right)

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
