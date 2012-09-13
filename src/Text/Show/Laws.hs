{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

module Text.Show.Laws where

import Test.ClassLaws
import Test.ClassLaws.TestingDatatypes

data ShowLaw s

class Show s => ShowLaws s where

  showLaw  ::  Law (ShowLaw s)

  showLaw  =   defaultShowLaw

defaultShowLaw (d,x,r,s)  =  showsPrec d x r ++ s  =.=  showsPrec d x (r ++ s)

type instance LawArgs (ShowLaw s)  =  (Int, s, String, String)
type instance LawBody (ShowLaw s)  =  String

instance (ShowLaws s) => LawTest (ShowLaw s) where
  lawtest _  =  testEqual . showLaw

