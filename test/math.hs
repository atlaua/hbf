{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns #-}

module Main where

import Control.Monad.State
import Control.Monad.Writer
import Test.QuickCheck

import HBF.Interpreter
import HBF.Parser
import HBF.PrgmIO
import HBF.Tape.CrumbList
import HBF.Types


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


newtype PurePrgmIO a = PurePrgmIO {getPurePrgmIO :: WriterT [Val] (State [Val]) a} deriving (Functor, Monad, MonadState [Val], MonadWriter [Val])

runPurePrgmIO :: [Val] -> PurePrgmIO a -> [Val]
runPurePrgmIO input = flip evalState input . execWriterT . getPurePrgmIO

instance PrgmIO PurePrgmIO where
    prgmRead = state (\(x:xs) -> (x, xs))
    prgmWrite x = tell [x]
