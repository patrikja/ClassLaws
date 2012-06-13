{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The core of ClassLaws are the type families 'LawArgs', 'LawBody' and
-- 'Param', tied together by the type class 'LawTest'. 
module Test.ClassLaws.Core where
import Test.QuickCheck 
import Test.ClassLaws.Partial

-- | An equality proof is represented as a list of (at least two) equal values.
type Equal    =  []     
-- | A Theorem is a claim that a LHS equals a RHS - an 'Equal' of length two.
type Theorem  =  Equal

infixr 0 =.=
-- | Contructing an equality theorem: @lhs =.= rhs  =  [lhs, rhs]@.
(=.=)        :: a -> a -> Theorem a 
lhs =.= rhs  =  [lhs, rhs]

-- | Take a two-element "theorem" and an equality proof chain to splice in the middle.
addSteps                    :: Theorem a -> Equal a -> Equal a
addSteps  [lhs,rhs]  steps  =  lhs : steps ++ [rhs]
addSteps  _          _      =  error "addSteps should only be used on two-element lists"


-- | The forall quantified part on the top level of the law
type family LawArgs t  
-- | The type in the body of the forall
type family LawBody t  
-- | Parameters needed for 'Equal' checking of the body
type family Param b    

-- | The 'Law's we handle are of this form.
type Law t  =  LawArgs t -> Equal (LawBody t)

{- |
Class LawTest defines a test method, which returns a testable property, which we
can use to test a law for a type t. This class is independent of the actual laws
to test - it can be used for Monoid, Monad, ...
-} 
class LawTest t where 
  lawtest :: t -> LawArgs t -> Param (LawBody t) -> Property

-- | Helper function to test laws where arguments lack a Show instance.
blindlawtest :: (LawTest t) => t -> Blind (LawArgs t) -> Param (LawBody t) -> Property
blindlawtest a (Blind f)  =  lawtest a f  

-- | Helper function to test laws where we should care about partial values.
partiallawtest :: (LawTest t) => t -> Partial ((LawArgs t) -> Param (LawBody t) -> Property)
partiallawtest a = Partial $ lawtest a

-- | Top level use of ClassLaws is often @'quickLawCheck' someLaw@
quickLawCheck ::
  (Show       (LawArgs t), 
   Arbitrary  (LawArgs t),
   Show       (Param (LawBody t)), 
   Arbitrary  (Param (LawBody t)), 
   LawTest t) =>
  t -> IO ()
quickLawCheck  =  quickCheck . lawtest
-- quickLawCheck  law  =  quickCheck (lawtest law) -- alternative version does not need expl. type sig.
-- | Variant not needing a Show instance for the 'LawArg's
quickFLawCheck law  =  quickCheck (blindlawtest  law)

-- | Checking laws in the precense of partial values
quickLawCheckPartial
  :: ( Show (Partial (Param (LawBody t)))
     , Show (Partial (LawArgs t))
     , ArbitraryPartial (Param (LawBody t))
     , ArbitraryPartial (LawArgs t)
     , LawTest t) =>
     t -> IO ()
quickLawCheckPartial =  quickCheck . Partial . lawtest

