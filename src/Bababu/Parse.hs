module Bababu.Parse where

import           Bababu.Types
import           Control.Monad.Free
import           Control.Monad.State
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as LBS8
import           Data.Maybe
import           Data.String
import           Text.HTML.TagSoup

type ParsingState = [Tag ByteString]

parse :: ByteString -> Either String (Program ByteString r)
parse xml =
  let tags    = parseTags xml
      (ns, _) = runState convertChildren tags
      fst []                 = Nothing
      fst (n@Element{} : _ ) = Just n
      fst (_           : ns) = fst ns
      n = fst ns
  in  case n of
        Nothing -> Left "Can't find any element in nodes."
        Just n  -> Right $ parseProgram [n]

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
          ++ LBS8.unpack t'
          ++ " by tag:"
          ++ LBS8.unpack t
          ++ "."
        else return n
      _ -> fail $ "Unexpected closing tag: " ++ LBS8.unpack t ++ "."

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

parseProgram :: IsString str => [Node str] -> Program str r
parseProgram []       = liftF Done
parseProgram (n : ns) = case n of
  Element t as cs -> Free $ Block t as (parseProgram cs) (parseProgram ns)
  Text txt        -> Free $ Expression txt (parseProgram ns)
