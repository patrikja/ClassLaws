-- | The central part of ClassLaws is defined in the .Core, .Partial
-- and .TestingEquality. Some more helper functions and examples
-- reside in Test.ClassLaws.*. Finally, laws for the Monoid, Monad and
-- MonadState classes live under their definitions in the hierarchy:
-- Data.Monoid.Laws, Control.Monad.Laws, etc.
module Test.ClassLaws ( module Test.ClassLaws.Core
                      , module Test.ClassLaws.Partial 
                      , module Test.ClassLaws.TestingEquality
                      , module Test.QuickCheck) where
import Test.ClassLaws.Core
import Test.ClassLaws.TestingEquality
import Test.ClassLaws.Partial hiding (Result)
import Test.QuickCheck 
