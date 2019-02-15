{-# LANGUAGE OverloadedStrings #-}
module Bababu.Render where

import           Bababu.Parse
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as LC8

render :: [Node ByteString] -> ByteString
render nodes = LBS.concat (map renderNode nodes)

renderNode :: Node ByteString -> ByteString
renderNode (Text txt) = LC8.pack $ "\"" ++ renderStmt (LC8.unpack txt) ++ "\""
renderNode (Element t attrs cs) =
  LBS.concat ["_h(\"", t, "\",{},", render cs, ")"]


renderStmt :: String -> String
renderStmt ('{' : '{' : bs) = "\" + " ++ renderStmt bs
renderStmt ('}' : '}' : bs) = "\"" ++ renderStmt bs
renderStmt (b         : bs) = b : renderStmt bs
renderStmt []               = []
