{-# LANGUAGE DeriveFunctor #-}

module Effs where

import FreerT

-- A library of basic effects

data Basic e s r = Get (s -> r) | Put s r | Fail e
  deriving Functor

get :: FreerT (Basic e s) m s
get = liftF $ Get id

put :: s -> FreerT (Basic e s) m ()
put s = liftF $ Put s ()

fail :: e -> FreerT (Basic e s) m x
fail e = liftF $ Fail e
