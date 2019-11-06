{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This uses typeclasses to create a "DSL".
-- It's not a very flexible DSL, because you can't make any
-- transformations on the program before interpreting it (no
-- access to some form of AST).
module ChatTypeClass where

import Data.Text
import Protolude

class Monad m => MonadChat m where

  invite :: Text -> m ()

  here :: m [Text]

  status :: Text -> m ()

-- | We can use `do` notation, beacuse Free is a Monad.
example :: MonadChat m => m ()
example = do
  invite "Lisa"
  users <- here
  invite "Bart"
  status "Working from home"

-- | Interpret a command into IO.
instance MonadChat IO where

  invite val = putStrLn ("You invited @" <> val <> "!")

  here = do
    let users = ["@Marge", "@Homer", "@Maggie"]
    putStrLn ("In this channel: " <> unwords users)
    pure users

  status val = putStrLn ("Setting status to '" <> val <> "'.")

--- $> program
program :: IO ()
program = example
