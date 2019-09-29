{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}

-- | Conctrete tests of some instances od the 'Monoid' laws (for
-- 'Endo', mainly). The laws themselves are one level up in the module
-- hierarchy: 'Data.Monoid.Laws.defaultMonoidLaw1' etc.
module Data.Monoid.Laws.Instances where

import Data.Monoid      (Monoid(mappend, mempty), Endo(Endo), appEndo)
import Data.Monoid.Laws (MonoidLaws(..), MonoidLaw1, MonoidLaw2, MonoidLaw3)

import Test.ClassLaws   ( Equal, Law, quickLawCheck, lawtest, Property, quickCheck
                        , Partial(Partial), unPartial
                        , ArbitraryPartial(arbitraryPartial), SemanticEq((==!), semanticEq), SemanticOrd
                        , quickLawCheckPartial
                        )

import Test.ClassLaws.TestingDatatypes (MyList(..), (+++))

import Test.ClassLaws.TestingFinFuns(arbitraryPartialFun, showPartialFun, eqPartial, semEqFun)

import Control.Monad (liftM)
import Data.List(intersperse)

instance MonoidLaws (Endo a)

-- | Cheating: just showing a few values (@map f [0..10]@).
instance Show (Endo Int) where
  show (Endo f) = "E("++(concat $ intersperse "," $ map (show . f) [0..10])++")"

testMonoidEndo = 
  do quickLawCheck  (undefined::MonoidLaw1 (Endo Bool))
     quickLawCheck  (undefined::MonoidLaw2 (Endo Bool))
     quickLawCheck  (undefined::MonoidLaw3 (Endo Bool))

instance (Bounded a, Enum a, Show (Partial a)) => Show (Partial (Endo a)) where
  show (Partial (Endo e)) = showPartialFun e
    
instance (Bounded a, Enum a, SemanticOrd a, 
          ArbitraryPartial a) => ArbitraryPartial (Endo a) where
  arbitraryPartial = liftM Endo (arbitraryPartialFun arbitraryPartial)
    
instance (Bounded a, Enum a, Eq a) => Eq (Endo a) where
  (Endo f) == (Endo g)  =  f == g

{-
-- Alternative definition, needs -- {-# LANGUAGE UndecidableInstances #-}
instance SemanticEq (a->a) => SemanticEq (Endo a) where
  semanticEq tweak (Endo f) (Endo g) = semanticEq tweak f g
-}
instance (Bounded a, Enum a, SemanticEq a) => SemanticEq (Endo a) where
  semanticEq tweak (Endo f) (Endo g) = semEqFun semanticEq tweak f g

testMonoidEndoPartial = do 
  quickLawCheckPartial (undefined::MonoidLaw1 (Endo Bool)) -- expected failure
  quickLawCheckPartial (undefined::MonoidLaw2 (Endo Bool)) -- expected failure
  quickLawCheckPartial (undefined::MonoidLaw3 (Endo Bool))

{-
The following Monoid instance for MyList does *not* satisfy the Monoid laws.
-}

instance Semigroup (MyList a) where
  xs <> ys  =  xs +++ ys +++ xs

instance Monoid (MyList a) where
  mempty  = Nil
  mappend = (<>)

instance MonoidLaws (MyList a)

testMonoidMyList = 
  do quickLawCheck  (undefined :: MonoidLaw1 (MyList Int))
     quickLawCheck  (undefined :: MonoidLaw2 (MyList Int))
     quickLawCheck  (undefined :: MonoidLaw3 (MyList Int))


main = do testMonoidEndo
          testMonoidMyList -- expected failures

-- ================================================================
-- Just for fun: Endo Bool is also finite and bounded ...

instance Bounded (Endo Bool) where
  minBound = Endo (const False)
  maxBound = Endo (const True)
  
instance Enum (Endo Bool) where
  fromEnum (Endo f) = 2*fromEnum (f False) + fromEnum (f True)
  toEnum n = Endo (\b->if b then toEnum(n`mod`2) else toEnum (n`div`2))
    -- cheating: should really check if n is within 0..3

b2i :: Bool -> Int
b2i = fromEnum

instance Show (Endo Bool) where
  show (Endo f) = 'E':concatMap (show.b2i.f) [False,True]
  
test_roundtrip :: Bool
test_roundtrip =  (toEnum :: Int -> Endo Bool) . (fromEnum :: Endo Bool -> Int) == id
