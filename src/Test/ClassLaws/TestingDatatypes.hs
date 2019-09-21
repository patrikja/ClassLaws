{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

-- | Some example usage of the ClassLaws framework
module Test.ClassLaws.TestingDatatypes where
import Control.Monad.State (State, runState, state, liftM)
import Data.Monoid (Endo(Endo))
import Test.ClassLaws

-- | To check equality of 'Endo'-functions we can generate argument values.
type instance Param (Endo a)  =  a

instance (SemanticEq (Endo a), Show (Partial (Endo a))) => TestEqual (Endo a) where
  testEqual l _ =  testEqPartial (==!) l

{-
instance (Eq ( a), Show (Partial a)) => TestEqual (Endo a) where
   testEqual = testRunEqPartial appEndo (==)
-}

-- | For lists, no 'Param'eter is needed, so we use @()@.
type instance Param [a] = ()

instance (Eq a, Show a) => TestEqual [a] where
  testEqual p _ = testEq (==) p

-- Char
type instance Param Char  =  ()

instance TestEqual Char where
  testEqual p _ = testEq (==) p

-- Maybe
type instance Param (Maybe a)  =  ()

instance (Eq a, Show a) => TestEqual (Maybe a) where
  testEqual p _ = testEq (==) p

-- State
type instance Param (State s a) = s

instance (Eq a, Show a, Eq s, Show s) => TestEqual (State s a) where
  testEqual = testRunEq runState (==) 

instance (CoArbitrary s, Arbitrary a, Arbitrary s) => Arbitrary (State s a) where
  arbitrary = fmap state arbitrary 

-- | We use the MyList datatype to provide instances that do not
-- satisfy some class laws.
data MyList a  = Cons a (MyList a) 
               | Nil 
                 deriving (Show, Eq)

foldrMyList                  :: (a -> b -> b) -> b -> MyList a -> b
foldrMyList f e Nil          =  e
foldrMyList f e (Cons x xs)  =  f x (foldrMyList f e xs)

list2MyList              :: [a] -> MyList a 
list2MyList []           =  Nil
list2MyList (x:xs)       =  Cons x (list2MyList xs)

myList2List              :: MyList a -> [a]
myList2List Nil          =  []
myList2List (Cons x xs)  =  x:myList2List xs

(+++)               :: MyList a -> MyList a -> MyList a
Nil +++ xs          =  xs
(Cons y ys) +++ xs  =  Cons y (ys +++ xs)

snoc                :: a -> MyList a -> MyList a
snoc y Nil          =  Cons y Nil
snoc y (Cons x xs)  =  Cons x (snoc y xs)

instance Arbitrary a => Arbitrary (MyList a) where
  arbitrary = fmap list2MyList arbitrary
  shrink = map list2MyList . shrink . myList2List

type instance Param (MyList a)  =  ()

instance (Eq a, Show a) => TestEqual (MyList a) where
  testEqual p _ = testEq (==) p
