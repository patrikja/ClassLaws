module Test.ClassLaws.Tests where
import Test.ClassLaws
import Test.ClassLaws.TestingFinFuns() -- Show instances for some partial functions

instance Arbitrary a => Arbitrary (Partial a) where
  arbitrary = fmap Partial $ genPartial 1 5 arbitrary

botB2B, cbotB2B :: Bool -> Bool
cbotB2B = const bottom
botB2B = bottom


test1 = testEqPartial (==!) [botB2B, cbotB2B]    -- should fail
test2 = testEqPartial (==!) [\x y -> y, seq :: Bool -> Bool -> Bool]  -- should fail
test3 = \(Partial x) -> testEqPartial (==!) [x, ()]                   -- should fail

main = do  quickCheck $ expectFailure $ test1
           quickCheck $ expectFailure $ test2
           quickCheck $ expectFailure $ test3
