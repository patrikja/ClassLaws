{-# LANGUAGE FlexibleContexts #-}

{- |

The following class + helper functions implement law-agnostic testing
functionality that is used to test laws for various classes.

-}

module Test.ClassLaws.TestingEquality where
import Test.QuickCheck.Property 
  
import Test.ClassLaws.Core(Equal, Param)
import Test.ClassLaws.Partial(Partial(Partial))

-- | A class for types which can be checked for 'Equal'ity, possibly
-- needing some extra 'Param'eters.
class TestEqual b where
  testEqual :: Equal b -> Param b -> Property

-- | The first function, 'testRunEq', returns a property implementing
-- an equality check.  It takes a function that can `run' a value and a
-- comparison operator to a predicate (which in turn takes some
-- supposedly equal values, and a parameter needed for the run
-- function, and returns a 'Property').
testRunEq :: Show r =>  (t -> p -> r) -> (r -> r -> Bool) -> 
                        (Equal t -> p -> Property)
testRunEq run (==) steps p = testEq (==) (map (`run` p) steps)

-- | The second function, 'testEq', does the same, but now for pairs
-- that are not necessarily runnable.
testEq :: Show a => (a -> a -> Bool) -> 
                    (Equal a -> Property)
testEq (==) steps = 
     whenFail (print      $  failingPair  (==)  steps)
  $  property $ liftBool  $  pairwiseEq   (==)  steps

----

-- | Variant of 'testRunEq' intended for 'Partial' values. (Only the
-- Show part differs - the user also needs to supply an equality
-- operator handling 'Partial' values.)
testRunEqPartial :: Show (Partial r) => 
  (t -> p -> r) -> (r -> r -> Bool) -> 
  (Equal t -> p -> Property)
testRunEqPartial run (==) steps p = testEqPartial (==) (map (`run` p) steps)

-- | Similar variant of 'testEq' for 'Partial' values.
testEqPartial :: Show (Partial a) => (a -> a -> Bool) -> Equal a -> Property
testEqPartial (==) steps = 
    whenFail (print $ Partial (failingPair  (==)  steps)) 
  $ property  $ liftBool      (pairwiseEq   (==)  steps)

----

-- | Local helper
pairwiseEq :: (r -> r -> Bool) -> [r] -> Bool
pairwiseEq (==) []        =  True
pairwiseEq (==) [x]       =  True
pairwiseEq (==) (x:y:ys)  =  x==y && pairwiseEq (==) (y:ys)

-- | Position in an equality proof
type Pos = Int

-- | Local helper
failingPair  :: (a -> a -> Bool) -> Equal a -> (Pos, a, a)
failingPair  =  failingPair' 1
-- | Local helper
failingPair' pos (==) (x:y:ys) = if not (x==y) 
                                 then (pos,x,y) 
                                 else failingPair' (1+pos) (==) (y:ys)

{- The following function generalises testEq and testRunEq
testRunEq :: Show r =>
  Maybe (p,r -> p -> r) -> (r -> r -> Bool) -> Equal r -> Property
testRunEq startrun (==) steps = 
  let run = case startrun of 
              Nothing           ->  id
              Just (start,run)  ->  flip run start
  in  whenFail (print (failingPair (==) (map run steps)))
    $ property 
    $ liftBool (pairwiseEq (==) (map run steps))

-- An instance of testRunEq
testEq :: Show a => (a -> a -> Bool) -> Equal a -> Property
testEq = testRunEq Nothing
-}
