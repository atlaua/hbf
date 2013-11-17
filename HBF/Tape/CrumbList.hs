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

instance Tape CrumbList where
    readCurVal = CrumbList $ fmap cur get
    writeCurVal v = CrumbList $ modify (\cl -> cl {cur = v})
    modifyCurVal f = CrumbList $ modify (\cl@CL {cur} -> cl {cur = f cur})

    moveLeft = CrumbList $ modify (\CL {..} -> CL { left = tail $ expand left
                                      , cur = head $ expand left
                                      , right = cur:right
                                      })
    moveRight = CrumbList $ modify (\CL {..} -> CL { left = cur:left
                                       , cur = head $ expand right
                                       , right = tail $ expand right
                                       })

runCrumbList :: CrumbList a -> [Val]
runCrumbList cl = getCL $ execState (getCrumbList cl) emptyCL

expand:: [Val] -> [Val]
expand [] = [0]
expand a = a