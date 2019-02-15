module Bababu.Parse where

import           Bababu.Types
import           Control.Monad.Free
import           Control.Monad.State
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as L8
import           Data.Maybe
import           Text.HTML.TagSoup

data Node str
  = Text str
  | Element str [(str, str)] [Node str]
  deriving Show

type ParsingState = [Tag ByteString]

parse :: ByteString -> Either String [Node ByteString]
parse xml =
  let tags   = parseTags xml
      (n, _) = runState convertChildren tags
  in  Right n

convert :: Maybe (Node ByteString) -> State ParsingState (Node ByteString)
convert node = do
  tags <- get
  when (length tags < 1) (fail "No tags to consume.")
  put $ tail tags
  case head tags of
    (TagOpen t as) -> do
      ns <- convertChildren
      convert . Just $ Element t as ns
    (TagClose t) -> case node of
      Just n@(Element t' _ _) -> if t' /= t
        then
          fail
          $  "Closing tag:"
          ++ L8.unpack t'
          ++ " by tag:"
          ++ L8.unpack t
          ++ "."
        else return n
      _ -> fail $ "Unexpected closing tag: " ++ L8.unpack t ++ "."

convertChildren :: State ParsingState [Node ByteString]
convertChildren = do
  tags <- get
  if length tags < 1
    then return []
    else case head tags of
      (TagOpen _ _) -> do
        n  <- convert Nothing
        ns <- convertChildren
        return $ n : ns
      (TagClose _  ) -> return []
      (TagText  txt) -> do
        put $ tail tags
        ns <- convertChildren
        return $ Text txt : ns


