{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}

-- | ClassLaws for the 'Monoid' class. Actual tests are defined in the Instances submodule and can be run from 'Data.Monoid.Laws.Instances.main'.
module Data.Monoid.Laws where
import Data.Monoid	
import Test.ClassLaws

data MonoidLaw1 m 
data MonoidLaw2 m 
data MonoidLaw3 m

class Monoid m => MonoidLaws m where

  monoidLaw1  ::  Law (MonoidLaw1 m)
  monoidLaw2  ::  Law (MonoidLaw2 m)
  monoidLaw3  ::  Law (MonoidLaw3 m)

  monoidLaw1  =  defaultMonoidLaw1
  monoidLaw2  =  defaultMonoidLaw2
  monoidLaw3  =  defaultMonoidLaw3

defaultMonoidLaw1 m           =                               m  =.=  m `mappend` mempty
defaultMonoidLaw2 m           =              m `mappend` mempty  =.=  m 
defaultMonoidLaw3 (m1,m2,m3)  =  m1 `mappend` (m2 `mappend` m3)  =.=  (m1 `mappend` m2) `mappend` m3   

type instance LawArgs (MonoidLaw1 m)  =  m
type instance LawBody (MonoidLaw1 m)  =  m

type instance LawArgs (MonoidLaw2 m)  =  m
type instance LawBody (MonoidLaw2 m)  =  m

type instance LawArgs (MonoidLaw3 m)  =  (m, m, m)
type instance LawBody (MonoidLaw3 m)  =  m

instance (MonoidLaws a, TestEqual a) => LawTest (MonoidLaw1 a) where
  lawtest _  =  testEqual . monoidLaw1

instance (MonoidLaws a, TestEqual a) => LawTest (MonoidLaw2 a) where
  lawtest _  =  testEqual . monoidLaw2

instance (MonoidLaws a, TestEqual a) => LawTest (MonoidLaw3 a) where
  lawtest _  =  testEqual . monoidLaw3
