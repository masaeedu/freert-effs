{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Effs where

import FreerT
import Data.Sum

-- A library of basic effects

data State s r = Get (s -> r) | Put s r
  deriving Functor

get :: (Apply Functor r, State s :< r) => FreerT (Sum r) m s
get = liftF . inject $ Get id

put :: (Apply Functor r, State s :< r) => s -> FreerT (Sum r) m ()
put s = liftF . inject $ Put s ()

data Except e r = Fail e
  deriving Functor

fail :: (Apply Functor r, Except e :< r) => e -> FreerT (Sum r) m ()
fail e = liftF . inject $ Fail e
