{-# LANGUAGE DeriveFunctor #-}
module Bababu.Types where

import           Control.Monad.Free

data StatementF next
  = ExpressionF String next
  | StatementIfF String (StatementF next) [StatementF next] next
  deriving Functor

type Statements = Free StatementF

