{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}

{- | This module implements the laws in Control.Monad, specified in
the Haskell 2010 report, in 6.3.5 for Functor, in 6.3.6 for Monad, and
in Chapter 13, module Control.Monad.  -}
module Control.Monad.Laws (module Test.ClassLaws, module Control.Monad.Laws) where
import Control.Monad -- For MonadPlus; Functor and Monad are both in the Prelude
import Test.ClassLaws (LawTest(lawtest), LawArgs, LawBody, Law, (=.=), TestEqual, testEqual)

data FunctorLaw1 a     (f :: * -> *) 
data FunctorLaw2 a b c (f :: * -> *) 

class Functor f => FunctorLaws f where
  
  functorLaw1  ::  Law (FunctorLaw1 a     f)
  functorLaw2  ::  Law (FunctorLaw2 a b c f)

  functorLaw1  =  defaultFunctorLaw1
  functorLaw2  =  defaultFunctorLaw2

defaultFunctorLaw1 x        =  fmap id x            =.=  id x
defaultFunctorLaw2 (f,g,x)  =  (fmap f . fmap g) x  =.=  fmap (f . g) x

type instance LawArgs (FunctorLaw1 a f)      =  f a
type instance LawBody (FunctorLaw1 a f)      =  f a

type instance LawArgs (FunctorLaw2 a b c f)  =  (b -> c, a -> b, f a)
type instance LawBody (FunctorLaw2 a b c f)  =  f c

instance  (FunctorLaws f, TestEqual (f a)) =>  LawTest (FunctorLaw1 a f) where
  lawtest _ =  testEqual . functorLaw1
         
instance  (FunctorLaws f, TestEqual (f c)) => LawTest (FunctorLaw2 a b c f) where
  lawtest _ =  testEqual . functorLaw2


data MonadLaw1 a b   (m :: * -> *)
data MonadLaw2 b     (m :: * -> *)
data MonadLaw3 b c d (m :: * -> *)

class Monad m => MonadLaws m where
  
  monadLaw1  ::  Law (MonadLaw1 a b   m)
  monadLaw2  ::  Law (MonadLaw2 b     m)
  monadLaw3  ::  Law (MonadLaw3 b c d m)
  
  monadLaw1  =  defaultMonadLaw1 
  monadLaw2  =  defaultMonadLaw2
  monadLaw3  =  defaultMonadLaw3

defaultMonadLaw1 (a,k)    =  return a >>= k           =.=  k a
defaultMonadLaw2 m        =  m >>= return             =.=  m
defaultMonadLaw3 (m,k,h)  =  m >>= (\x -> k x >>= h)  =.=  (m >>= k) >>= h

type instance LawArgs (MonadLaw1 a b   m)  =  (a, a -> m b) 
type instance LawBody (MonadLaw1 a b   m)  =  m b

type instance LawArgs (MonadLaw2 b     m)  =  m b
type instance LawBody (MonadLaw2 b     m)  =  m b

type instance LawArgs (MonadLaw3 b c d m)  =  (m b, b -> m c, c -> m d)
type instance LawBody (MonadLaw3 b c d m)  =  m d

instance (MonadLaws m,TestEqual (m b)) => LawTest (MonadLaw1 a b m) where
  lawtest _  =  testEqual . monadLaw1
         
instance (MonadLaws m,TestEqual (m b)) => LawTest (MonadLaw2 b m) where
  lawtest _  =  testEqual . monadLaw2

instance (MonadLaws m,TestEqual (m d)) => LawTest (MonadLaw3 b c d m) where
  lawtest _  =  testEqual . monadLaw3


data FunctorMonadLaw a b (m :: * -> *)

class (Functor m, Monad m) => FunctorMonadLaws m where

  functorMonadLaw :: Law (FunctorMonadLaw a b m)

  functorMonadLaw  =  defaultFunctorMonadLaw 

defaultFunctorMonadLaw (f,xs)  =  fmap f xs  =.=  xs >>= return . f


type instance LawArgs (FunctorMonadLaw a b m) = (a -> b, m a) 
type instance LawBody (FunctorMonadLaw a b m) = m b
 
instance (FunctorMonadLaws m,TestEqual (m b)) => LawTest (FunctorMonadLaw a b m) where
  lawtest _  =  testEqual . functorMonadLaw


{- | The laws for MonadPlus are less prominently declared in the base
libraries. -}

data MonadPlusLaw1 a   (m :: * -> *)
data MonadPlusLaw2 a   (m :: * -> *)
data MonadPlusLaw3 a b (m :: * -> *)
data MonadPlusLaw4 a   (m :: * -> *)
data MonadPlusLaw5 a   (m :: * -> *)

class MonadPlus m => MonadPlusLaws m where

  monadPlusLaw1 :: Law (MonadPlusLaw1 a   m)
  monadPlusLaw2 :: Law (MonadPlusLaw2 a   m)
  monadPlusLaw3 :: Law (MonadPlusLaw3 a b m)
  monadPlusLaw4 :: Law (MonadPlusLaw4 a   m)
  monadPlusLaw5 :: Law (MonadPlusLaw5 a   m)
  
  monadPlusLaw1  =  defaultMonadPlusLaw1
  monadPlusLaw2  =  defaultMonadPlusLaw2
  monadPlusLaw3  =  defaultMonadPlusLaw3
  monadPlusLaw4  =  defaultMonadPlusLaw4
  monadPlusLaw5  =  defaultMonadPlusLaw5

defaultMonadPlusLaw1 x        =  mzero `mplus` x          =.=  x 
defaultMonadPlusLaw2 x        =  x `mplus` mzero          =.=  x
defaultMonadPlusLaw3 f        =  mzero >>= f              =.=  mzero
defaultMonadPlusLaw4 v        =  v >> mzero               =.=  mzero
defaultMonadPlusLaw5 (a,b,c)  =  a `mplus` (b `mplus` c)  =.=  (a `mplus` b) `mplus` c

type instance LawArgs (MonadPlusLaw1 a m)    =  m a 
type instance LawBody (MonadPlusLaw1 a m)    =  m a

type instance LawArgs (MonadPlusLaw2 a m)    =  m a 
type instance LawBody (MonadPlusLaw2 a m)    =  m a

type instance LawArgs (MonadPlusLaw3 a b m)  =  a -> m b 
type instance LawBody (MonadPlusLaw3 a b m)  =  m b
 
type instance LawArgs (MonadPlusLaw4 a m)    =  m a 
type instance LawBody (MonadPlusLaw4 a m)    =  m a

type instance LawArgs (MonadPlusLaw5 a m)    =  (m a, m a, m a)
type instance LawBody (MonadPlusLaw5 a m)    =  m a

instance (MonadPlusLaws m,TestEqual (m a)) => LawTest (MonadPlusLaw1 a m) where
  lawtest _  =  testEqual . monadPlusLaw1

instance (MonadPlusLaws m,TestEqual (m a)) => LawTest (MonadPlusLaw2 a m) where
  lawtest _  =  testEqual . monadPlusLaw2 

instance (MonadPlusLaws m,TestEqual (m b)) => LawTest (MonadPlusLaw3 a b m) where
  lawtest _  =  testEqual . monadPlusLaw3

instance (MonadPlusLaws m,TestEqual (m a)) => LawTest (MonadPlusLaw4 a m) where
  lawtest _  =  testEqual . monadPlusLaw4

instance (MonadPlusLaws m,TestEqual (m a)) => LawTest (MonadPlusLaw5 a m) where
  lawtest _  =  testEqual . monadPlusLaw5
