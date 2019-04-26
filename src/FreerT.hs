{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE DeriveFunctor             #-}

module FreerT where

import Control.Monad.Trans.Class

import Control.Monad.Trans.Cont
import Control.Monad (ap)

data FreerInput f r a = Done a | forall x. More (f x) (x -> r)

instance Functor (FreerInput f r) where
  fmap f (Done a)   = Done (f a)
  fmap _ (More x y) = More x y

instance Applicative (FreerInput f r) where
  pure = Done
  (<*>) = ap

instance Monad (FreerInput f r) where
  (Done a)   >>= f = f a
  (More x y) >>= _ = More x y

newtype FreerT f m a = FreerT { runFreerT :: forall r. ContT r m (FreerInput f (m r) a) }
  deriving Functor

instance Applicative (FreerT f m) where
  pure a = FreerT $ pure . pure $ a
  (<*>) = ap

instance Monad (FreerT f m) where
  (FreerT ma) >>= amb = FreerT $ ma >>= \case
    Done a -> runFreerT $ amb a
    More x y -> pure $ More x y

instance MonadTrans (FreerT f) where
  lift ma = FreerT $ ContT $ \cb -> ma >>= cb . Done

wrap :: f (FreerT f m a) -> FreerT f m a
wrap fr = FreerT $ ContT $ \cb -> cb (More fr (\ft -> (runContT . runFreerT) ft cb))

liftF :: Functor f => f a -> FreerT f m a
liftF = wrap . fmap pure

iterT :: (Functor f, Monad m) => (f (m a) -> m a) -> FreerT f m a -> m a
iterT phi (FreerT (ContT m)) = m $ \case
  Done a -> pure a
  More f xmr -> phi $ xmr <$> f
