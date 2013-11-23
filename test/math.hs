{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Test.QuickCheck

import HBF.Interpreter
import HBF.Parser
import HBF.PrgmIO.Pure
import HBF.Tape.CrumbList


main :: IO ()
main = mapM_ (\t -> putStrLn (name t ++ ":") >> quickCheck (runBinaryTest t)) binaryTests

binaryTests :: [BinaryTest]
binaryTests = [ BinaryTest {name = "add", bf = ",>,[-<+>]<.", fun = (+)}
              , BinaryTest {name = "mul", bf = ",>,<[->[->+>+<<]>[-<+>]<<]>>>.", fun = (*)}
              ]


newtype AVal = AVal Val

instance Arbitrary AVal where
    arbitrary = fmap AVal $ choose (0, 50)

instance Show AVal where
    show (AVal v) = show v


data BinaryTest = BinaryTest {name :: String, bf :: String, fun :: Val -> Val -> Val}

runBinaryTest :: BinaryTest -> AVal -> AVal -> Bool
runBinaryTest BinaryTest {bf, fun} (AVal a) (AVal b) = bfRes == funRes
    where funRes = fun a b
          bfRes = head . runPurePrgmIO [a, b] . runCrumbListT . runCmds . getRight $ parseBF bf
          getRight (Right x) = x
