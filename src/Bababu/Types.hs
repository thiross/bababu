{-# LANGUAGE DeriveFunctor #-}
module Bababu.Types where

import           Control.Monad.Free
import           Data.ByteString.Lazy           ( ByteString )

data StatementF next
  = ExpressionF ByteString next
  | StatementIfF ByteString (StatementF next) [StatementF next] next
  deriving Functor

type Statements = Free StatementF

