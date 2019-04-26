{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Sum

import qualified Control.Monad.State as MS
import qualified Control.Monad.Except as ME

import FreerT
import Effs

-- Test computation pls ignore

blacklist :: [String]
blacklist = ["berthold"]

bouncer :: (Apply Functor r, '[State String, Except String] :<: r) => FreerT (Sum r) m ()
bouncer = do
  v <- get
  if v `elem` blacklist
  then Effs.fail $ "sorry, no " ++ v ++ "s allowed"
  else put $ "welcome back, " ++ v

-- For convenience, we interpret everything into mtl monads

interpret :: (Apply Functor r, '[State String, Except String] :<: r, MS.MonadState s m, ME.MonadError e m) => FreerT (Sum r) m a -> m a
interpret = iterT $ _ 
-- \case
--   Get f   -> MS.get >>= f
--   Put s r -> MS.put s >> r
--   Fail e  -> ME.throwError e

-- Go, go, go!

runAndPrint :: String -> MS.StateT String (ME.Except String) a -> IO ()
runAndPrint s = print . ME.runExcept . flip MS.execStateT s

main :: IO ()
main = do
  runAndPrint "berthold" $ interpret bouncer
  runAndPrint "alice"    $ interpret bouncer
