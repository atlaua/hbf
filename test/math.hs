{-# LANGUAGE RecordWildCards #-}

module Main where

import Test.QuickCheck

import HBF.Interpreter
import HBF.Optimizer
import HBF.Parser
import HBF.PrgmIO.Pure
import HBF.Tape.CrumbList
import HBF.Test


main :: IO ()
main = mapM_ (\t -> putStrLn (name t ++ ":") >> quickCheck (runBinaryTest t)) binaryTests

binaryTests :: [BinaryTest]
binaryTests = [ BinaryTest{name = "add_opt", opt = True,  bf = bfAdd, fun = (+)}
              , BinaryTest{name = "mul_opt", opt = True,  bf = bfMul, fun = (*)}
              , BinaryTest{name = "add_raw", opt = False, bf = bfAdd, fun = (+)}
              , BinaryTest{name = "mul_raw", opt = False, bf = bfMul, fun = (*)}
              ]


newtype AVal = AVal Val

instance Arbitrary AVal where
    arbitrary = fmap AVal $ choose (0, 50)

instance Show AVal where
    show (AVal v) = show v


data BinaryTest = BinaryTest { name :: String
                             , opt  :: Bool
                             , bf   :: String
                             , fun  :: Val -> Val -> Val
                             }

runBinaryTest :: BinaryTest -> AVal -> AVal -> Bool
runBinaryTest BinaryTest{..} (AVal a) (AVal b) = bfRes == funRes
    where funRes = fun a b
          bfRes = head $ runTestBF opt [a, b] bf
          getRight (Right x) = x
