{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Using GADTs to build a DSL.
-- I learned about this from https://deque.blog/2017/12/08/continuation-passing-style-free-monads-and-direct-style-free-monads/.
-- It basically adds pure and bind into your DSL to allow you to
-- create a monad based on that.
module ChatGADT where

import Data.Text
import Protolude

data Command a where
  -- GADTs allow us to have nice and expressive signatures.
  -- Instead of the somewhat alien type constructors we need with Free.
  Invite :: Text -> Command ()
  Here :: Command [Text]
  Status :: Text -> Command ()
  -- Monad api at a type level
  -- pure :: a -> m a
  Pure :: a -> Command a
  -- (>>=) :: m a -> (a -> m b) -> m b
  Bind :: Command a -> (a -> Command b) -> Command b

-- We can simply implement Functor, Applicative and Monad in terms of Pure and Bind.
instance Functor Command where
  fmap :: (a -> b) -> Command a -> Command b
  fmap f g = Bind g (Pure . f)

instance Applicative Command where

  pure :: a -> Command a
  pure = Pure

  (<*>) :: Command (a -> b) -> Command a -> Command b
  (<*>) f g = Bind f (<$> g)

instance Monad Command where
  (>>=) :: Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

-- | We can use `do` notation, beacuse Command is a Monad.
example :: Command ()
example = do
  Invite "Lisa" -- We don't need any helper functions for syntactic sugar.
  users <- Here
  Invite "Bart"
  Status "Working from home"

-- | Interpret a command into IO.
--- $> interpret example
interpret :: Command a -> IO a
interpret command =
  case command of
    Pure a -> pure a
    -- The recursion is handled in one place!
    Bind f g -> interpret f >>= interpret . g
    Invite val -> putStrLn ("You invited @" <> val <> "!")
    Here -> do
      let users = ["@Marge", "@Homer", "@Maggie"]
      putStrLn ("In this channel: " <> unwords users)
      pure users
    Status val -> putStrLn ("Setting status to '" <> val <> "'.")
