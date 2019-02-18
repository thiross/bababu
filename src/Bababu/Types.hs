{-# LANGUAGE DeriveFunctor #-}
module Bababu.Types where

import           Control.Monad.Free
import           Data.ByteString.Lazy           ( ByteString )

data Node str
  = Text str
  | Element str [(str, str)] [Node str]
  deriving Show

data StatementF str r
  = Done
  | Expression str r
  | Block str [(str, str)] (Program str r) r
  | IfBlock str (Program str r) (Program str r) r
  deriving Functor

type Program str = Free (StatementF str)
