{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Using Free to build a DSL.
-- This allows us to have a construct to make transformation
-- before interpreting it, while having a nice DSL with syntactic
-- sugar.
module ChatFree where

import Data.Text
import Free (Free)
import qualified Free
import Protolude

data Command next -- next command (recurrsion)
  = Invite Text next
  | -- This commands passes [Text] to the next command
    Here ([Text] -> next)
  | Status Text next
  deriving (Functor) -- Needs DeriveFunctor

-- | Syntactic sugar
invite :: Text -> Free Command ()
invite val = Free.liftF (Invite val ())

here :: Free Command [Text]
here = Free.liftF (Here identity)

status :: Text -> Free Command ()
status val = Free.liftF (Status val ())

-- | We can use `do` notation, beacuse Free is a Monad.
example :: Free Command ()
example = do
  invite "Lisa"
  users <- here
  invite "Bart"
  status "Working from home"

-- | Interpret a command into IO.
--- $> Free.fold interpret example
interpret :: Command a -> IO a
interpret command =
  case command of
    Invite val next -> do
      putStrLn ("You invited @" <> val <> "!")
      pure next
    Here next -> do
      let users = ["@Marge", "@Homer", "@Maggie"]
      putStrLn ("In this channel: " <> unwords users)
      pure (next users)
    Status val next -> do
      putStrLn ("Setting status to '" <> val <> "'.")
      pure next
