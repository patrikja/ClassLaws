{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}

-- | Laws for the 'MonadState' class. A submodule has a
-- 'Control.Monad.State.Class.Laws.Instances.main' which runs quite a
-- few tests for the lazy and strict state monads.
module Control.Monad.State.Class.Laws where
import Control.Monad.State.Class (MonadState(..))
import Test.ClassLaws

class MonadState s m  => MonadStateLaws s m where
    monadStatePutPut :: Law (MonadStatePutPut s m)
    monadStatePutGet :: Law (MonadStatePutGet s m)
    monadStateGetPut :: Law (MonadStateGetPut m)
    monadStateGetGet :: Law (MonadStateGetGet s a m)

    monadStatePutPut = defaultMonadStatePutPut
    monadStatePutGet = defaultMonadStatePutGet
    monadStateGetPut = defaultMonadStateGetPut
    monadStateGetGet = defaultMonadStateGetGet

defaultMonadStatePutPut (s,s')  =               put s' >>  put s  =.=  put s
defaultMonadStatePutGet s       =               put s  >>  get    =.=  put s >> return s
defaultMonadStateGetPut _       =               get    >>= put    =.=  return ()
defaultMonadStateGetGet k       =      get >>= (\s->get >>= k s)  =.=  get >>= \s->k s s

data MonadStatePutPut s   (m :: * -> *)
data MonadStatePutGet s   (m :: * -> *)
data MonadStateGetPut     (m :: * -> *)
data MonadStateGetGet s a (m :: * -> *)

type instance LawArgs (MonadStatePutPut s m)    =  (s, s) 
type instance LawBody (MonadStatePutPut s m)    =  m ()

type instance LawArgs (MonadStatePutGet s m)    =  s
type instance LawBody (MonadStatePutGet s m)    =  m s

type instance LawArgs (MonadStateGetPut m)      =  () 
type instance LawBody (MonadStateGetPut m)      =  m ()

type instance LawArgs (MonadStateGetGet s a m)  =  s -> s -> m a
type instance LawBody (MonadStateGetGet s a m)  =  m a


instance (MonadStateLaws s m, TestEqual (m ())) =>
    LawTest (MonadStatePutPut s m) where
  lawtest _ = testEqual . (monadStatePutPut :: Law (MonadStatePutPut s m))
--  lawtest _ = testEqual . monadStatePutPut -- explicit type needed

instance (MonadStateLaws s m, TestEqual (m s)) =>
    LawTest (MonadStatePutGet s m) where
  lawtest _ = testEqual . (monadStatePutGet :: Law (MonadStatePutGet s m))

instance (MonadStateLaws s m, TestEqual (m ())) =>
    LawTest (MonadStateGetPut m) where
  lawtest _ = testEqual . (monadStateGetPut :: Law (MonadStateGetPut m))

instance (MonadStateLaws s m, TestEqual (m a)) =>
    LawTest (MonadStateGetGet s a m) where
  lawtest _ = testEqual . (monadStateGetGet :: Law (MonadStateGetGet s a m))
