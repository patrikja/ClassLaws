{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}

-- | Functions from a finite type can be shown, checked for equality,
-- and generated. We provide variants both for total and for partial
-- values.
module Test.ClassLaws.TestingFinFuns where
import Test.QuickCheck
import Test.ClassLaws.Partial
  (Partial(..), ArbitraryPartial(arbitraryPartial), genPartial
  , bottom, isBottom
  , SemanticEq ( (==!), semanticEq ), Tweak
  , SemanticOrd ( (<=!), semanticCompare, (/\!), semanticMeet, semanticJoin )
  )
import Data.List (intersperse)
import Control.Monad (forM)

showPartialFun ::
  (Bounded a, Enum a, Show (Partial b), Show (Partial a)) =>
  (a -> b) -> String
showPartialFun f = 
  if    isBottom f 
  then  "<_bot_a->b_>"
  else  "<(" ++
        (concat $ intersperse "; "
           [  show  (Partial x) ++ "->" ++ 
              show  (Partial (f x))
           |  x <- (bottom:enumElems)])
        ++ ")>"


showFun :: (Enum a, Bounded a, Show a, Show b) => (a -> b) -> String
showFun f = "<(" ++ (concat $ intersperse "; "
                                [ show x ++ "->" ++ show (f x)
                                  | x <- enumElems])
                            ++ ")>"

enumElems :: (Bounded a, Enum a) => [a]
enumElems = [minBound .. maxBound]


arbitraryPartialFun :: forall e a. 
  (Enum e, Bounded e, SemanticOrd a) => 
  Gen a -> Gen (e -> a)
arbitraryPartialFun ag = do
  funtab <- forM (bottom : enumElems :: [e]) (\_ -> ag)
  genPartial 1 10 (return (table2fun funtab))

type FunTab e a = [a]

table2fun :: (Enum e, Bounded e, SemanticOrd a) => 
  FunTab e a -> (e -> a)
table2fun tab@(_:tottab) = fun
  where meet = lMeet tab
        fun x  | isBottom x  = meet 
               | otherwise   = tail tottab !! (fromEnum x)

lMeet :: (SemanticOrd a) => [a] -> a
lMeet []      =  bottom
lMeet [x]     =  x
lMeet (x:xs)  =  x /\! lMeet xs

------------------------------------------------------------

instance (Enum a, Bounded a, Show a, Show b) =>
    Show (a->b) where
        show = showFun

instance  ( Enum e, Bounded e, Eq e                      
          , SemanticOrd s, ArbitraryPartial s            
          ) => ArbitraryPartial (e -> s) where           
  arbitraryPartial = arbitraryPartialFun arbitraryPartial

instance  (  Enum e, Bounded e
          ,  Show (Partial e), Show (Partial b)
          ) => Show (Partial (e->b)) where
  show (Partial f) = showPartialFun f
------------------------------------------------------------

semanticLE _ a b = case ( isBottom a, isBottom b ) of
  (True, _)      -> True
  _              -> False
------------------------------------------------------------

instance (Bounded a, Enum a, Eq      b) => Eq      (a->b) where
  f == g = all (\x -> f x == g x) enumElems

instance (Bounded a, Enum a, SemanticEq   b) => SemanticEq   (a->b) where
  semanticEq = semEqFun semanticEq
  
type SemEq a = Tweak->a->a->Bool
semEqFun :: (Bounded a, Enum a) => SemEq b -> SemEq (a->b)
semEqFun semEqB tweak f g = eqPartial (all (\x -> semEqB tweak (f x) (g x)) 
                                           (bottom : enumElems)) 
                                      f g

instance (Bounded a, Enum a, SemanticOrd  b) => SemanticOrd  (a->b) where
  semanticCompare tweak f g =
      case ( semanticEq tweak f g
           , isBottom f
           , isBottom g ) of
        (True,  _,     _)     -> Just EQ
        (_,     True,  _)     -> Just LT
        (_,     _,     True)  -> Just GT
        (_,     _,     _)     -> 
            if lessEqPartial (all (\x -> f x <=! g x) enumElems) f g then
                Just LT
            else if lessEqPartial (all (\x -> g x <=! f x) enumElems) f g then
                     Just GT
                 else
                     Nothing
  semanticJoin tweak f g = undefined
  -- semanticJoin tweak f g = case (isBottom f, isBottom g) of
  --   (True,  True)  -> Just bottom
  --   (True,  False) -> Just g
  --   (False, True)  -> Just f
  --   (False, False) -> (\x -> (\/!) (f x) (g x))
                     -- propagate Nothing here how?
  semanticMeet tweak f g = case (isBottom f, isBottom g) of
    (False, False) -> \x -> (/\!) (f x) (g x)
    (_,     _)     -> bottom

------------------------------------------------------------


lessEqPartial nonBotLE x y = case (isBottom x, isBottom y) of
  (True,  _)     -> True
  (False, True)  -> False    
  (False, False) -> nonBotLE

eqPartial nonBotEq x y = case (isBottom x, isBottom y) of
  (True,  True)  -> True
  (False, False) -> nonBotEq
  _              -> False    

meetPartial q x y = case (isBottom x, isBottom y) of
  (False, False)  ->  q
  _               ->  bottom

