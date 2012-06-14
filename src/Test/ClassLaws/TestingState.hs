{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Implementations of the infrastructure needed to test state monad
-- laws.
module Test.ClassLaws.TestingState where
import Test.ClassLaws
{-
-- More details about the imports.
import Test.QuickCheck
import Test.ChasingBottoms
    ( bottom, isBottom
    , SemanticEq ( (==!), semanticEq )
    , SemanticOrd ( semanticCompare, semanticMeet, semanticJoin )
    )
import Test.ClassLaws.Partial
  (Partial(..), ArbitraryPartial(arbitraryPartial), genPartial)
import Test.ClassLaws.TestingEquality
  ( TestEqual(testEqual)
  , testEqPartial,    testEq
  , testRunEqPartial, testRunEq)
-}

import Test.ClassLaws.TestingFinFuns

import Control.Monad.State.Class(MonadState(..))
import Control.Monad (liftM, liftM2)

import Data.Data

data Pair a b = Pair a  b

fstP ~(Pair a b) = a
sndP ~(Pair a b) = b

newtype State s a = S {runS :: s -> Pair a s}

getState :: State s s
getState = S $ \s -> Pair s s

putState :: s -> State s ()
putState s = S $ const (Pair () s)

returnState a = S $ \s -> Pair a s


bindStateL m k = S $ \s ->  let (Pair a s') = runS m s
                            in runS (k a) s'

fmapStateL f m = S $ \s ->  let (Pair a s') = runS m s
                            in (Pair (f a) s')


bindStateS m k = S $ \s -> case runS m s of
                             (Pair a s') -> runS (k a) s'

fmapStateS f m = S $ \s -> case runS m s of
                             (Pair a s') -> (Pair (f a) s')

------------------------------------------------------------

pairFromGen :: Gen a -> Gen b -> Gen (Pair a b)
pairFromGen a b = (liftM2 Pair a b)

pairShowPartial :: String -> Pair a b -> String
pairShowPartial _   p | isBottom p = "_|_P_"
pairShowPartial pab (Pair a b) = pab

basicPairShow :: (a-> String) -> (b -> String) -> Pair a b -> String
basicPairShow sa sb (Pair a b) = "("++sa a++", "++sb b++")"

instance  (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = pairFromGen arbitrary arbitrary

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (Pair a b) where
    coarbitrary (Pair a b) = variant 1 . coarbitrary a . coarbitrary b

instance (Show a, Show b) => Show (Pair a b) where
    show = basicPairShow show show

instance  (ArbitraryPartial a, ArbitraryPartial b) =>
    ArbitraryPartial (Pair a b) where
        arbitraryPartial = genPartial 1 9 $ pairFromGen arbitraryPartial arbitraryPartial

instance (Show (Partial a), Show (Partial b)) =>
    Show (Partial (Pair a b)) where
        show (Partial p) = pairShowPartial (basicPairShow (show.Partial) (show.Partial) p) p

------------------------------------------------------------

instance ( Arbitrary a
         , Arbitrary s
         , CoArbitrary s
         ) => Arbitrary (State s a) where
  arbitrary = liftM S arbitrary

instance  (Enum s, Bounded s, Show a, Show s) =>
          Show (State s a) where
  show (S f) = "(S " ++ show f ++ ")"

instance  ( ArbitraryPartial a, SemanticOrd a
          , ArbitraryPartial s, SemanticOrd s 
          , Enum s, Bounded s, Eq s 
          ) => ArbitraryPartial (State s a) where
  arbitraryPartial = genPartial 1 20 (liftM S arbitraryPartial)

instance (Enum s, Bounded s, Show (Partial a), Show (Partial s)) =>
    Show (Partial (State s a)) where
        show (Partial s) | isBottom s = "_|_St_"
        show (Partial (S f)) = "(S " ++ show (Partial f) ++ ")"

------------------------------------------------------------

instance (Eq a, Eq b) => Eq (Pair a b) where
    px == py = pairRecPatt (==) (==) (&&) px py

instance (SemanticEq a, SemanticEq b) => SemanticEq (Pair a b) where
    semanticEq tweak x y =
        -- case ( isBottomTimeOut (timeOutLimit tweak) x
        --      , isBottomTimeOut (timeOutLimit tweak) y ) of
        case ( isBottom x, isBottom y ) of
          (True, True)   -> True
          (False, False) ->
              ((semanticEq tweak) (fstP x) (fstP y)) &&
               ((semanticEq tweak) (sndP x) (sndP y))
          _              -> False

instance (SemanticOrd a, SemanticOrd b) => SemanticOrd (Pair a b) where
  semanticCompare tweak x y =
      case ( semanticEq tweak x y
           , isBottom x
           , isBottom y ) of
        (True,  _,     _)     -> Just EQ
        (_,     True,  _)     -> Just LT
        (_,     _,     True)  -> Just GT
        (_,     _,     _)     -> 
            case (l == r) of
              True  -> l
              _     -> Nothing
            where
              l = semanticCompare tweak (fstP x) (fstP y)
              r = semanticCompare tweak (sndP x) (sndP y)
  semanticJoin tweak x y = case (isBottom x, isBottom y) of
    (True,  True)  -> Just bottom
    (True,  False) -> Just y
    (False, True)  -> Just x
    -- (False, True)  -> cast x
    (False, False) -> case ( semanticJoin tweak (fstP x) (fstP y)
                           , semanticJoin tweak (sndP x) (sndP y)) of
                        (Nothing,   _)         -> Nothing
                        (_,         Nothing)   -> Nothing
                        (Just fst,  Just snd)  -> Just $ Pair fst snd
  semanticMeet tweak x y = case (isBottom x, isBottom y) of
    (True,  _)     -> bottom
    (_,     True)  -> bottom
    (False, False) -> Pair (semanticMeet tweak (fstP x) (fstP y))
                      (semanticMeet tweak (sndP x) (sndP y))

-- -- semanticLE _tweak a b = case ( isBottomTimeOut (timeOutLimit tweak) a
-- --                              , isBottomTimeOut (timeOutLimit tweak) b ) of
-- semanticLE _ a b = case ( isBottom a, isBottom b ) of
--   (True, _)      -> True
--   _              -> False

pairRecPatt :: (a->a->ta) -> (b->b->tb) -> (ta->tb->t) -> Pair a b -> Pair a b -> t
pairRecPatt opA opB topOp px py = topOp (opA (fstP px) (fstP py)) (opB (sndP px) (sndP py))

------------------------------------------------------------

instance (Enum a, Bounded a, Eq a, Eq b) => Eq (State a b) where
  (==) x y = eqPartial q x y
    where q = statePatt (==) x y
    
instance (Enum a, Bounded a, SemanticEq a, SemanticEq b) => SemanticEq (State a b) where
    semanticEq tweak x y = eqPartial q x y
        where q = statePatt (semanticEq tweak) x y
instance (Enum a, Bounded a, SemanticOrd a, SemanticOrd b) => SemanticOrd (State a b) where
  semanticCompare tweak x y =
      case ( semanticEq tweak x y
           , isBottom x
           , isBottom y ) of
        (True,  _,     _)     -> Just EQ
        (_,     True,  _)     -> Just LT
        (_,     _,     True)  -> Just GT
        (_,     _,     _)     -> statePatt (semanticCompare tweak) x y
  semanticJoin tweak x y = error "TODO: semanticJoin for State not yet implemented"
  -- semanticJoin tweak f g = case (isBottom f, isBottom g) of
  --   (True,  True)  -> Just bottom
  --   (True,  False) -> Just g
  --   (False, True)  -> Just f
  --   (False, False) -> (\x -> (\/!) (f x) (g x))
                     -- propagate Nothing here how?
  semanticMeet tweak x y = case (isBottom x, isBottom y) of
    (False, False) -> S $ statePatt (semanticMeet tweak) x y
    (_,     _)     -> bottom

statePatt op (S f1) (S f2) = op f1 f2

------------------------------------------------------------

instance Arbitrary Ordering where
    arbitrary = enumTotArb $ zip [1,1,1] $ enumElems

instance CoArbitrary Ordering where
    coarbitrary = coarbitrary . fromEnum

instance ArbitraryPartial Ordering where
    arbitraryPartial = genPartial 1 9 $ enumTotArb $
                       zip [1,1,1] $ enumElems

instance Show (Partial Ordering) where
    show = enumShowBot_auxLst ["Ord", "LT", "EQ", "GT"] . unPartial

enumTotArb :: [(Int,a)] -> Gen a
enumTotArb as = frequency $ map (\(f,a) -> (f,return a)) as

enumShowBot_auxLst :: (Bounded a, Enum a) => [String] -> a -> String
enumShowBot_auxLst (s:ss) x | isBottom x = "_|_"++s++"_"
enumShowBot_auxLst (s:ss) x = ss !! fromEnum x





-- instances for the lazy state
instance Monad (State s) where
  return  =  returnState
  (>>=)   =  bindStateL

instance Functor (State s) where
  fmap    =  fmapStateL

instance Monad (State s) => MonadState s (State s) where
  get     =  getState
  put     =  putState


instance ( SemanticEq a, Show (Partial a)
         , SemanticEq s, Show (Partial s)
         , Bounded s, Enum s) => TestEqual (State s a) where
  testEqual eq _ = testEqPartial (==!) (map runS eq)

type instance Param (State s a) = s

----------------------------------------------------------------------
newtype SS s a = SS {unSS :: State s a}
    deriving( Arbitrary, Show, ArbitraryPartial, MonadState s
            , SemanticEq, SemanticOrd)
-- deriving instance (Show (Partial a), Show (Partial s), Bounded s, Enum s
--                   , SemanticEq a, SemanticEq s) => TestEqual (SS s a)

instance ( SemanticEq a, Show (Partial a)
         , SemanticEq s, Show (Partial s)
         , Bounded s, Enum s) => TestEqual (SS s a) where
  testEqual eq _ = testEqPartial (==!) (map unSS eq)

instance (Enum s, Bounded s, Show (Partial a), Show (Partial s)) =>
    Show (Partial (SS s a)) where
        show (Partial (SS x)) = show (Partial x)


instance Monad (SS s) where
  return        =  SS . returnState
  (SS m) >>= k  =  bindSS 
      where
        bindSS = SS $ S $ \s -> case runS m s of
                                  (Pair a s') -> x s'
                                      where
                                        SS (S x) = k a

instance Functor (SS s) where
  fmap  f (SS m) = SS $ S $ \s -> case runS m s of
                                    (Pair a s') -> (Pair (f a) s')

type instance Param (SS    s a) = s
