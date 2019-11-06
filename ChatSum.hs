{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Example for combining union types or in our case DSLs.
-- I've learned about this from
-- http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf
-- Swierstra, Wouter. "Data types a la carte." Journal of functional
-- programming 18.4 (2008): 423-436.
module ChatSum where

import qualified ChatFree as Chat
import Data.Text
import Free (Free)
import qualified Free
import Protolude hiding (Sum, log)

-- | Sum is like Either, but for types.
-- This allows us to combine two types that both take an `a`.
data Sum f g a
  = InL (f a)
  | InR (g a)
  deriving (Functor)

data Logging next
  = Log Text next
  deriving (Functor)

type Program = Sum Chat.Command Logging

log :: Text -> Free Program ()
log val = Free.liftF (InR $ Log val ())

invite :: Text -> Free Program ()
invite val = Free.hoist InL (Chat.invite val)

here :: Free Program [Text]
here = Free.hoist InL Chat.here

status :: Text -> Free Program ()
status val = Free.hoist InL (Chat.status val)

example :: Free Program ()
example = do
  log "Start"
  invite "Lisa"
  users <- here
  invite "Bart"
  status "Working from home"
  log "End"

-- | Interpret a program into IO.
--- $> Free.fold interpret example
interpret :: Program a -> IO a
interpret command =
  case command of
    InL x -> Chat.interpret x
    InR x -> interpretLogging x

interpretLogging :: Logging a -> IO a
interpretLogging command =
  case command of
    Log val next -> do
      putStrLn ("Log: " <> val)
      pure next
