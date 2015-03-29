{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Tests of the 'MonadState' laws for lazy and strict state monads.
-- The laws are one level up in the module hierarchy:
-- 'Control.Monad.State.Class.Laws.defaultMonadStatePutGet' etc.
module Main where

import Control.Monad.Laws
  ( MonadLaws, FunctorLaws, FunctorMonadLaws
  , MonadLaw1,   MonadLaw2,   MonadLaw3
  , FunctorLaw1, FunctorLaw2, FunctorMonadLaw
  )
import Control.Monad.State.Class.Laws

import Test.ClassLaws
import Test.ClassLaws.TestingState


instance MonadStateLaws s  (State s)
instance MonadLaws         (State s)
instance FunctorLaws       (State s)
instance FunctorMonadLaws  (State s)


instance MonadStateLaws s  (SS s)
instance MonadLaws         (SS s)
instance FunctorLaws       (SS s)
instance FunctorMonadLaws  (SS s)
----------------------------------------------------------------------

testLawsStateL :: IO ()
testLawsStateL = do
  putStr "*         MonadStatePutPut Bool       (State Bool): "; quickLawCheck        (undefined::MonadStatePutPut Bool       (State Bool))
  putStr "*         MonadStatePutGet Bool       (State Bool): "; quickLawCheck        (undefined::MonadStatePutGet Bool       (State Bool))
  putStr "*         MonadStateGetPut            (State Bool): "; quickLawCheck        (undefined::MonadStateGetPut            (State Bool))
  putStr "*         MonadStateGetGet Bool Three (State Bool): "; quickLawCheck        (undefined::MonadStateGetGet Bool Three (State Bool))
  putStr "*         FunctorLaw1 ()              (State Bool): "; quickLawCheck        (undefined::FunctorLaw1 ()              (State Bool))
  putStr "*         FunctorLaw2 Three Bool ()   (State Bool): "; quickLawCheck        (undefined::FunctorLaw2 Three Bool ()   (State Bool))
  putStr "*         MonadLaw1 Bool ()           (State Bool): "; quickLawCheck        (undefined::MonadLaw1 Bool ()           (State Bool))
  putStr "*         MonadLaw2 Three             (State Bool): "; quickLawCheck        (undefined::MonadLaw2 Three             (State Bool))
  putStr "*         MonadLaw3 () Three Bool     (State Bool): "; quickLawCheck        (undefined::MonadLaw3 () Three Bool     (State Bool))
  putStr "*         FunctorMonadLaw () Three    (State Bool): "; quickLawCheck        (undefined::FunctorMonadLaw () Three    (State Bool))

testLawsStatePartialL :: IO ()
testLawsStatePartialL = do
  putStr "* Partial MonadStatePutPut Bool       (State Bool): "; quickLawCheckPartial (undefined::MonadStatePutPut Bool       (State Bool))
  putStr "* Partial MonadStatePutGet Bool       (State Bool): "; quickLawCheckPartial (undefined::MonadStatePutGet Bool       (State Bool))
  putStr "* Partial MonadStateGetPut            (State Bool): "; quickLawCheckPartial (undefined::MonadStateGetPut            (State Bool))
  putStr "* Partial MonadStateGetGet Bool Three (State Bool): "; quickLawCheckPartial (undefined::MonadStateGetGet Bool Three (State Bool))
  putStr "* Partial FunctorLaw1 ()              (State Bool): "; quickLawCheckPartial (undefined::FunctorLaw1 ()              (State Bool))
  putStr "* Partial FunctorLaw2 Three Bool ()   (State Bool): "; quickLawCheckPartial (undefined::FunctorLaw2 Three Bool ()   (State Bool))
  putStr "* Partial MonadLaw1 Bool ()           (State Bool): "; quickLawCheckPartial (undefined::MonadLaw1 Bool ()           (State Bool))
  putStr "* Partial MonadLaw2 Three             (State Bool): "; quickLawCheckPartial (undefined::MonadLaw2 Three             (State Bool))
  putStr "* Partial MonadLaw3 () Three Bool     (State Bool): "; quickLawCheckPartial (undefined::MonadLaw3 () Three Bool     (State Bool))
  putStr "* Partial FunctorMonadLaw () Three    (State Bool): "; quickLawCheckPartial (undefined::FunctorMonadLaw () Three    (State Bool))

testLawsStateS :: IO ()
testLawsStateS = do
  putStr "*         MonadStatePutPut Bool       (SS    Bool): "; quickLawCheck        (undefined::MonadStatePutPut Bool       (SS    Bool))
  putStr "*         MonadStatePutGet Bool       (SS    Bool): "; quickLawCheck        (undefined::MonadStatePutGet Bool       (SS    Bool))
  putStr "*         MonadStateGetPut            (SS    Bool): "; quickLawCheck        (undefined::MonadStateGetPut            (SS    Bool))
  putStr "*         MonadStateGetGet Bool Three (SS    Bool): "; quickLawCheck        (undefined::MonadStateGetGet Bool Three (SS    Bool))
  putStr "*         FunctorLaw1 ()              (SS    Bool): "; quickLawCheck        (undefined::FunctorLaw1 ()              (SS    Bool))
  putStr "*         FunctorLaw2 Three Bool ()   (SS    Bool): "; quickLawCheck        (undefined::FunctorLaw2 Three Bool ()   (SS    Bool))
  putStr "*         MonadLaw1 Bool ()           (SS    Bool): "; quickLawCheck        (undefined::MonadLaw1 Bool ()           (SS    Bool))
  putStr "*         MonadLaw2 Three             (SS    Bool): "; quickLawCheck        (undefined::MonadLaw2 Three             (SS    Bool))
  putStr "*         MonadLaw3 () Three Bool     (SS    Bool): "; quickLawCheck        (undefined::MonadLaw3 () Three Bool     (SS    Bool))
  putStr "*         FunctorMonadLaw () Three    (SS    Bool): "; quickLawCheck        (undefined::FunctorMonadLaw () Three    (SS    Bool))

testLawsStatePartialS :: IO ()
testLawsStatePartialS = do
  putStr "* Partial MonadStatePutPut Bool       (SS    Bool): "; quickLawCheckPartial (undefined::MonadStatePutPut Bool       (SS    Bool))
  putStr "* Partial MonadStatePutGet Bool       (SS    Bool): "; quickLawCheckPartial (undefined::MonadStatePutGet Bool       (SS    Bool))
  putStr "* Partial MonadStateGetPut            (SS    Bool): "; quickLawCheckPartial (undefined::MonadStateGetPut            (SS    Bool))
  putStr "* Partial MonadStateGetGet Bool Three (SS    Bool): "; quickLawCheckPartial (undefined::MonadStateGetGet Bool Three (SS    Bool))
  putStr "* Partial FunctorLaw1 ()              (SS    Bool): "; quickLawCheckPartial (undefined::FunctorLaw1 ()              (SS    Bool))
  putStr "* Partial FunctorLaw2 Three Bool ()   (SS    Bool): "; quickLawCheckPartial (undefined::FunctorLaw2 Three Bool ()   (SS    Bool))
  putStr "* Partial MonadLaw1 Bool ()           (SS    Bool): "; quickLawCheckPartial (undefined::MonadLaw1 Bool ()           (SS    Bool))
  putStr "* Partial MonadLaw2 Three             (SS    Bool): "; quickLawCheckPartial (undefined::MonadLaw2 Three             (SS    Bool))
  putStr "* Partial MonadLaw3 () Three Bool     (SS    Bool): "; quickLawCheckPartial (undefined::MonadLaw3 () Three Bool     (SS    Bool))
  putStr "* Partial FunctorMonadLaw () Three    (SS    Bool): "; quickLawCheckPartial (undefined::FunctorMonadLaw () Three    (SS    Bool))

main :: IO ()
main = do
  testLawsStateL
  testLawsStatePartialL
  testLawsStateS
  testLawsStatePartialS
