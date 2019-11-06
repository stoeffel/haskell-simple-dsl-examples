{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

-- | A very simple implementation of Free.
-- Checkout https://hackage.haskell.org/package/free for a
-- complete implementation.
module Free
  ( Free (..),
    liftF,
    fold,
    hoist,
  )
where

-- | I like the analogy to List made by Gabriel Gonzalez.
--
--   data List a   = Cons  a (List a  )  | Nil
--   data Free f a = Free (f (Free f a)) | Pure a
--
-- "We can think of a free monad as just being a list of
-- functors." -- Gabriel Gonzalez
data Free f a
  = Free {unFree :: f (Free f a)}
  | Pure a
  deriving (Functor)

-- | Helper function to lift some functor into Free.
liftF :: Functor f => f a -> Free f a
liftF = Free . fmap Pure

-- | Fold any functor into some monad.
fold :: Monad m => (forall b. f b -> m b) -> Free f a -> m a
fold f free =
  case free of
    Pure x -> return x
    Free x -> do
      y <- f x
      fold f y

hoist :: Functor g => (forall a. f a -> g a) -> Free f b -> Free g b
hoist f free =
  case free of
    Pure x -> Pure x
    Free x -> Free (hoist f <$> f x)

-- Instances for getting a Monad for free.

instance Functor f => Applicative (Free f) where

  pure = Pure

  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free b = Free $ fmap a <$> b
  Free a <*> b = Free $ (<*> b) <$> a

instance (Functor f) => Monad (Free f) where

  return = Pure

  (Free x) >>= f = Free (fmap (>>= f) x)
  (Pure a) >>= f = f a
