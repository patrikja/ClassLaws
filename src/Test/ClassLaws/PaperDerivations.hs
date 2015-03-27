module Test.ClassLaws.PaperDerivations where
import Test.ClassLaws.TestingState
import Test.ChasingBottoms (SemanticEq, (==!), isBottom, bottom)

{-
import Control.Monad.Laws
  ( MonadLaws(..), FunctorLaws(..), FunctorMonadLaws
  , defaultFunctorLaw1
  , MonadLaw1, MonadLaw2, MonadLaw3
  , FunctorLaw1, FunctorLaw2, FunctorMonadLaw
  )
import Control.Monad.State.Class(MonadState(..))
import Control.Monad.State.Class.Laws
import Test.ClassLaws
import Test.ClassLaws.TestingEquality
   (TestEqual(testEqual), testEqPartial)
-}

fLaw1eeL =
    [ fmap id (bottom :: State Bool ())
    , -- definition of fmap
      S $ \s -> let  Pair a s' = runS bottom s
                in   Pair a s'
    , -- apply runS
      S $ \s -> let  Pair a s' = bottom
                in   Pair a s'
    , -- let reduction
      S $ \s -> Pair bottom bottom
    , -- ** where it breaks (5)
      S bottom
    , -- newtype strict constructor S
      bottom
    , -- apply id
      id bottom
    ]

fLaw1reL =
    [
     (bottom:: Bool -> Pair () Bool) True
    , -- ===  -- apply bottom
     bottom:: Pair () Bool
    , -- ** ==/=
     Pair (bottom:: ()) (bottom:: Bool)
    , -- ===  -- beta reduction
     (\s -> Pair (bottom:: ()) (bottom:: Bool)) True
    ]

fLaw1eeS =
    [
      fmap id (bottom:: SS Bool ())
    , -- ===  -- definition of fmap
      SS $ S $ \s ->  case runS (bottom::State Bool ()) s of
                   Pair a s'  ->  Pair (id a) s'
    , -- ===  -- apply runS and id
      SS $ S $ \s ->  case (bottom::Pair () Bool) of
                   Pair a s'  ->  Pair a s'
    , -- ===  -- case reduction
      SS $ S $ \s ->  (bottom::Pair () Bool)
    , -- ** ==/=
      (bottom:: SS Bool ())
    , -- ===  -- apply id
      id (bottom:: SS Bool ())
    ]

mLaw1eeS = let k = const bottom in
    [
      return False >>= k
    , -- ===  -- definition of bind
      SS $ S $ \s ->  case runS (return False) s of
                        Pair a s' -> runS (k a) s'
    , -- ===  -- definition of return
      SS $ S $ \s ->  case runS (S $ \s -> Pair False s) s of
                        Pair a s' -> runS (k a) s'
    , -- ===  -- apply runS
      SS $ S $ \s ->  case (\s -> Pair False s) s of
                        Pair a s' -> runS (k a) s'
    , -- ===  -- beta reduction
      SS $ S $ \s ->  case Pair False s of
                        Pair a s' -> runS (k a) s'
    , -- ===  -- case reduction
      SS $ S $ \s ->  (runS.unSS) (k False) s
    , -- ===  -- apply k
      SS $ S $ \s ->  (runS.unSS) (bottom::SS Bool ()) s
    , -- ===  -- apply runS
      SS $ S $ \s ->  bottom::Pair () Bool
    , -- ** ==/=
      bottom::SS Bool ()
    , -- ===  -- apply k
      k False
    ]


mLaw1eeL = let k = bottom::Bool -> State Bool () in
    [
      return False >>= k
    , -- ===  -- definition of bind
      S $ \s ->  let  Pair a s' = runS (return False) s
                 in   runS (k a) s'
    , -- ===  -- definition of return
      S $ \s ->  let  Pair a s' = runS (S $ \s -> Pair False s) s
                 in   runS (k a) s'
    , -- ===  -- apply runS
      S $ \s ->  let  Pair a s' = (\s -> Pair False s) s
                 in   runS (k a) s'
    , -- ===  -- beta reduction
      S $ \s ->  let  Pair a s' = Pair False s
                 in   runS (k a) s'
    , -- ===  -- let reduction
      S $ \s ->  runS (k False) s
    , -- ===  -- apply k
      S $ \s ->  runS (bottom::State Bool ()) s
    , -- ===  -- apply runS
      S $ \s ->  bottom::Pair () Bool
    , -- ** ==/=
      bottom::State Bool ()
    , -- ===  -- apply k
      k False
    ]

mLaw2reL =
    [
      runS (m >>= return) bottom
    , -- ===
      Pair (bottom::Three) (bottom::Bool)
    , -- ** ==/=
      bottom::Pair Three Bool
    , -- ===
      runS m bottom
    ]
  where m = bottom :: State Bool Three

mLaw2eeL =
    [
     m >>= return
    , -- ===
     S $ const (Pair (bottom::Three) (bottom::Bool))
    , -- ** ==/=
     bottom :: State Bool Three
    , -- ===
     m
    ]
  where m = bottom :: State Bool Three


mLaw2eeS =
    [
     m >>= return
    , -- ===
     SS $ S $ const (bottom :: Pair (Three) (Bool))
    , -- ** ==/=
     bottom :: SS Bool Three
    , -- ===
     m
    ]
  where m = bottom :: SS Bool Three

main = do
    printResults $ runSteps fLaw1eeL
    printResults $ runSteps fLaw1reL
    printResults $ runSteps fLaw1eeS
    printResults $ runSteps mLaw1eeS
    printResults $ runSteps mLaw1eeL
    printResults $ runSteps mLaw2reL
    printResults $ runSteps mLaw2eeL
    printResults $ runSteps mLaw2eeS

runSteps :: SemanticEq a => [a] -> [Bool]
runSteps xs = zipWith (==!) xs (tail xs)

printResults = putStrLn . showBList

showBList = map (\b->if b then '.' else 'F')
