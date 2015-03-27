{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
-- | This module collects the infrastructure used to easily switch
-- between testing ClassLaws with or without partial values. Built
-- around QuickCheck and ChasingBottoms.
module Test.ClassLaws.Partial
       ( module Test.ClassLaws.Partial
       , module Test.ChasingBottoms
       ) where
-- A bug in ghc-7.2 complains about this part (re-export thinks Result still clashes)
import Test.QuickCheck
import Test.ChasingBottoms hiding (Result, listOf, infiniteListOf) -- clash with QuickCheck

import Data.List (intersperse)
import Control.Monad (liftM2, liftM3)

-- | A modifier to indicate that partial values should be generated
-- (or tested, or both).
newtype Partial a  =  Partial {unPartial :: a}

instance TestablePartial prop => Testable (Partial prop) where
  property (Partial x) = propertyPartial x

-- | Declaring a property for possibly partial values.
class TestablePartial prop where
  propertyPartial   ::  prop -> Property

-- | We copy the QuickCheck structure to make sure generators of
-- partial values and equality checks handling partial values are
-- used.
class ArbitraryPartial a where
  arbitraryPartial  ::  Gen a

  shrinkPartial     ::  a -> [a]
  shrinkPartial _   =   []


instance TestablePartial Bool where
  propertyPartial = property

instance TestablePartial Property where
  propertyPartial = property

instance ( ArbitraryPartial a
         , Show (Partial a)
         , TestablePartial prop
         ) => TestablePartial (a -> prop) where
  propertyPartial f = forAllShrink arb shr prop
      where
        arb               = fmap Partial arbitraryPartial
        shr  (Partial x)  = map  Partial (shrinkPartial x)
        prop (Partial x)  = propertyPartial (f x)

--------------------------------------------------------------

-- | Helper for showing partial values
showPartial :: String -> (a -> String) -> a -> String
showPartial  t  _  p  | isBottom p  =  "_|_" ++ t ++ "_"
showPartial  _  f  p                =  f p

instance  Show (Partial ())   where
  show (Partial u)  =  showPartial "()"    show  u

instance  Show (Partial Bool) where
  show (Partial b)  =  showPartial "Bool"  show  b

instance  Show (Partial Char) where
  show (Partial c)  =  showPartial "Char"  show  c

instance  Show (Partial Int)  where
  show (Partial i)  =  showPartial "Int"   show  i

-- | Helper for generating partial values: @genPartial ib ia ga@
-- generates 'bottom' with frequence @ib@ and @ga@ with frequency
-- @ia@.
genPartial :: Int -> Int -> Gen a -> Gen a
genPartial ib ia ga = frequency [ (ib, return bottom), (ia, ga) ]

instance ArbitraryPartial Int where
  arbitraryPartial  =  genPartial 1 20  $ arbitrary

instance ArbitraryPartial Char where
  arbitraryPartial  =  genPartial 1 20  $ arbitrary

instance ArbitraryPartial Bool where
  arbitraryPartial  =  genPartial 1 10  $ arbitrary

instance ArbitraryPartial () where
  arbitraryPartial  =  genPartial 1 5   $ arbitrary

------------------------------------------------------------

instance (Show (Partial a), Show (Partial b)) => Show (Partial (a,b)) where
  show = showPartial "(,)" showPair
    where showPair (Partial (a,b)) =
            "(" ++ show (Partial a) ++ ","
                ++ show (Partial b) ++ ")"

instance (ArbitraryPartial a, ArbitraryPartial b) => ArbitraryPartial (a,b) where
  arbitraryPartial = liftM2 (,) arbitraryPartial arbitraryPartial

instance (Show (Partial a), Show (Partial b), Show (Partial c)) => Show (Partial (a,b,c)) where
  show = showPartial "(,)" showTriple
    where showTriple (Partial (a,b,c)) =
            "(" ++ show (Partial a) ++ ","
                ++ show (Partial b) ++ ","
                ++ show (Partial c) ++ ")"

instance (ArbitraryPartial a, ArbitraryPartial b, ArbitraryPartial c) => ArbitraryPartial (a,b,c) where
    arbitraryPartial = liftM3 (,,) arbitraryPartial arbitraryPartial arbitraryPartial
