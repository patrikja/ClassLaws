{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Tests of the 'MonadState' laws for lazy and strict state monads.
-- The laws are one level up in the module hierarchy:
-- 'Control.Monad.State.Class.Laws.defaultMonadStatePutGet' etc.
module Control.Monad.State.Class.Laws.Instances where

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

-- NOTE: This module has moved to test/Main.hs and only remains here in case somebody depends on it.

testLawsStateL = do
  quickLawCheck (undefined::MonadStatePutPut Bool (State Bool))
  quickLawCheck (undefined::MonadStatePutGet Bool (State Bool))
  quickLawCheck (undefined::MonadStateGetPut (State Bool))
  quickLawCheck (undefined::MonadStateGetGet Bool Three (State Bool))
  quickLawCheck (undefined::FunctorLaw1 () (State Bool))
  quickLawCheck (undefined::FunctorLaw2 Three Bool () (State Bool))
  quickLawCheck (undefined::MonadLaw1 Bool () (State Bool))
  quickLawCheck (undefined::MonadLaw2 Three (State Bool))
  quickLawCheck (undefined::MonadLaw3 () Three Bool (State Bool))
  quickLawCheck (undefined::FunctorMonadLaw () Three (State Bool))

testLawsStatePartialL = do
  quickLawCheckPartial (undefined::MonadStatePutPut Bool (State Bool))
  quickLawCheckPartial (undefined::MonadStatePutGet Bool (State Bool))
  quickLawCheckPartial (undefined::MonadStateGetPut (State Bool))
  quickLawCheckPartial (undefined::MonadStateGetGet Bool Three (State Bool))
  quickLawCheckPartial (undefined::FunctorLaw1 () (State Bool))
  quickLawCheckPartial (undefined::FunctorLaw2 Three Bool () (State Bool))
  quickLawCheckPartial (undefined::MonadLaw1 Bool () (State Bool))
  quickLawCheckPartial (undefined::MonadLaw2 Three (State Bool))
  quickLawCheckPartial (undefined::MonadLaw3 () Three Bool (State Bool))
  quickLawCheckPartial (undefined::FunctorMonadLaw () Three (State Bool))

testLawsStateS = do
  quickLawCheck (undefined::MonadStatePutPut Bool (SS Bool))
  quickLawCheck (undefined::MonadStatePutGet Bool (SS Bool))
  quickLawCheck (undefined::MonadStateGetPut (SS Bool))
  quickLawCheck (undefined::MonadStateGetGet Bool Three (SS Bool))
  quickLawCheck (undefined::FunctorLaw1 () (SS Bool))
  quickLawCheck (undefined::FunctorLaw2 Three Bool () (SS Bool))
  quickLawCheck (undefined::MonadLaw1 Bool () (SS Bool))
  quickLawCheck (undefined::MonadLaw2 Three (SS Bool))
  quickLawCheck (undefined::MonadLaw3 () Three Bool (SS Bool))
  quickLawCheck (undefined::FunctorMonadLaw () Three (SS Bool))

testLawsStatePartialS = do
  quickLawCheckPartial (undefined::MonadStatePutPut Bool (SS Bool))
  quickLawCheckPartial (undefined::MonadStatePutGet Bool (SS Bool))
  quickLawCheckPartial (undefined::MonadStateGetPut (SS Bool))
  quickLawCheckPartial (undefined::MonadStateGetGet Bool Three (SS Bool))
  quickLawCheckPartial (undefined::FunctorLaw1 () (SS Bool))
  quickLawCheckPartial (undefined::FunctorLaw2 Three Bool () (SS Bool))
  quickLawCheckPartial (undefined::MonadLaw1 Bool () (SS Bool))
  quickLawCheckPartial (undefined::MonadLaw2 Three (SS Bool))
  quickLawCheckPartial (undefined::MonadLaw3 () Three Bool (SS Bool))
  quickLawCheckPartial (undefined::FunctorMonadLaw () Three (SS Bool))

main = do
  testLawsStateL
  testLawsStatePartialL
  testLawsStateS
  testLawsStatePartialS
