{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Tests the Monad ClassLaws for a few example datatypes. Mainly
-- instance declarations and QuickCheck tests + a 'main' to run it.
module Control.Monad.Laws.Instances where
import Control.Monad.State
import Control.Monad.Laws
import Test.ClassLaws
import Test.ClassLaws.TestingDatatypes(MyList(..), (+++), snoc, foldrMyList)

-- take a proof of equality of b parametrised by a and make it a proof
--   of equality of functions from a to b. Assumes the length of the
--   proof is independent of the parameter a. (Requires irrefutable
--   patterns in the parametrised proof.)

-- TODO: The proof lengths are _not_ independent of the input in our
-- case below.

makeFunListOf :: (a->Equal b) -> Equal (a->b)
makeFunListOf f = map (\i a-> f a !! i) [0 .. listLength-1]
  where listLength = length (f undefined)

type instance Param (a->b) = a
        
instance FunctorLaws [] where
  functorLaw1 dummy = addSteps (defaultFunctorLaw1 dummy)
         (makeFunListOf $ \xs -> case xs of
            []        ->  nilCase 
            ys@(_:_)  ->  conCase ys)
    where
     nilCase = 
       [ fmap id []            
       , -- definition of fmap on []
         []
       ]
     conCase (y:ys) = 
       [ fmap id (y:ys)
       , -- definition of fmap on (x:xs)
         id y:fmap id ys
       , -- definition of id
         y:fmap id ys 
--         y:fmap id (ys++ys) -- gives an error (used to test error injection)
       , -- induction hypothesis
         y:ys
       , -- definition of id
        id (y:ys)
       ]

testFunctorList
  = do quickLawCheck   (undefined::FunctorLaw1 Char [])
       quickFLawCheck  (undefined::FunctorLaw2 Int Char Bool [])

instance FunctorLaws Maybe

testFunctorMaybe 
  = do quickLawCheck   (undefined::FunctorLaw1 Char Maybe)
       quickFLawCheck  (undefined::FunctorLaw2 Int Char Bool Maybe)


instance FunctorLaws IO

{- -- How do I test IO values?

testFunctorIO 
  = do quickBlind (undefined::FunctorLaw1 Char IO)
       quickBlind (undefined::FunctorLaw2 Int Char Bool IO)
-}


{- 
The following instance of Functor for MyList should *not* satisfy the functor
laws.
-}

-- Wrong instance of functor, because the order is reversed by fmap.

instance Functor MyList where
  fmap f Nil          =  Nil
  fmap f (Cons x xs)  =  snoc (f x) (fmap f xs)

instance FunctorLaws MyList
  where
  functorLaw1 xs = addSteps (defaultFunctorLaw1 xs)
         (case xs of
            Nil             ->  nilCase 
            zs@(Cons y ys)  ->  conCase zs)
    where
     nilCase = 
       [ fmap id Nil            
       , -- definition of fmap on []
         Nil
       ]
     conCase (Cons y ys) = 
       [ fmap id (Cons y ys)
       , -- definition of fmap on (x:xs)
         snoc (id y) (fmap id ys)
       , -- definition of id
         snoc y (fmap id ys)
       , -- induction hypothesis
         snoc y ys
       , -- definition of id
         id (Cons y ys)
       ]

testFunctorMyList
  = do quickLawCheck  (undefined::FunctorLaw1 Int MyList)
       quickFLawCheck (undefined::FunctorLaw2 Char Int Int MyList)


instance MonadLaws [] 

testMonadList
  = do quickFLawCheck (undefined::MonadLaw1 Char Int [])
       quickLawCheck  (undefined::MonadLaw2 Int [])
       quickFLawCheck (undefined::MonadLaw3 Int Bool Char [])


instance MonadLaws Maybe 

testMonadMaybe
  = do quickFLawCheck (undefined::MonadLaw1 Char Int Maybe)
       quickLawCheck  (undefined::MonadLaw2 Int Maybe)
       quickFLawCheck (undefined::MonadLaw3 Int Bool Char Maybe)


instance FunctorMonadLaws MyList

testFunctorMonadMyList
  = do quickFLawCheck (undefined:: FunctorMonadLaw Char Int MyList)


instance MonadLaws IO


instance MonadLaws (State s)

testMonadState
  = do quickFLawCheck (undefined::MonadLaw1 Bool Int (State Bool))
       quickFLawCheck (undefined::MonadLaw2      Int (State Bool)) -- necessary because of Show State problem
       quickFLawCheck (undefined::MonadLaw3 Int Bool Char (State Bool))


instance  Monad MyList  where
    m >>= k             = foldrMyList ((+++) . k) Nil m
    m >> k              = foldrMyList ((+++) . (\ _ -> k)) Nil m
    return x            = Cons x (Cons x Nil) -- gives an error
--    return x            = Cons x Nil  -- correct
    fail _              = Nil

instance MonadLaws MyList 
 
testMonadMyList
  = do quickFLawCheck (undefined::MonadLaw1 Char Int MyList)
       quickLawCheck  (undefined::MonadLaw2 Int MyList)
       quickFLawCheck (undefined::MonadLaw3 Int Bool Char MyList)


instance FunctorMonadLaws [] 

testFunctorMonadList
  = do quickFLawCheck (undefined::FunctorMonadLaw Char Int [])


instance FunctorMonadLaws Maybe

testFunctorMonadMaybe
  = do quickFLawCheck (undefined::FunctorMonadLaw Char Int Maybe)


instance FunctorMonadLaws IO


main = do testMonadMaybe
          testMonadState

          testFunctorList
          testFunctorMaybe

          testFunctorMonadList
          testFunctorMonadMaybe

expectedFailures = do
  testMonadMyList    
  testFunctorMyList 
  testFunctorMonadMyList         

-- No MonadPlusLaw instances yet. 

