module Bababu.Parse where

import           Bababu.Types
import           Control.Monad.Free
import           Data.ByteString
import           Xeno.DOM

template :: ByteString -> Either String (Statements ())
template xml = case parse xml of
  Left  e -> Left $ show e
  Right n -> Right $ statements n

statements :: Node -> Statements ()
statements node =
  liftF $ ExpressionF "ABC" ()

  
  



