{-# LANGUAGE OverloadedStrings #-}
module Bababu.Render where

import           Bababu.Parse
import           Control.Monad.State
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as LBS8

render :: [Node ByteString] -> ByteString
render nodes = LBS.concat (map renderNode nodes)

renderNode :: Node ByteString -> ByteString
renderNode (Text txt) = LBS8.pack . concat $ expr
 where
  (expr, _) = runState
    (stmt $ LBS8.unpack txt)
    (StmtState {readPos = 0, readState = Out, getStmts = [], getTop = ""})
renderNode (Element tag attrs cs) =
  LBS.concat $ ["_h(", tag] ++ map renderNode cs ++ [")"]

data ReadState
  = In
  | Out
  | InOpen Int
  | Open Int
  | Close Int
  deriving Show

data StmtState = StmtState
  { readPos   :: Int
  , readState :: ReadState
  , getStmts  :: [String]
  , getTop    :: String
  } deriving Show

stmt :: String -> State StmtState [String]
stmt src = do
  s <- get
  let pos = readPos s
  if pos >= length src
    then finishStmt
    else do
      let c = src !! pos
      put $ s { readPos = pos + 1 }
      case c of
        '{' -> consumeOpenBrace src
        '}' -> consumeCloseBrace src
        _   -> do
          consumeChar src c
          stmt src

finishStmtBy :: String -> String -> String -> State StmtState [String]
finishStmtBy prefix postfix extra = do
  s <- get
  let top = getTop s
  if null top && null extra
    then do
      put $ s { readState = Out }
      return . getStmts $ s
    else do
      let r = getStmts s ++ [prefix ++ top ++ extra ++ postfix]
      put $ s { readState = Out, getStmts = r, getTop = "" }
      return r

finishStmt :: State StmtState [String]
finishStmt = do
  s <- get
  case readState s of
    Close 2 -> finishStmtBy "(" ")" ""
    Out     -> finishStmtBy "\"" "\"" ""
    Open 1  -> finishStmtBy "\"" "\"" "{"
    Open _  -> fail "Expected '}}' not found."
    In      -> fail "Expected '}}' not found."

consumeOpenBrace :: String -> State StmtState [String]
consumeOpenBrace src = do
  s <- get
  case readState s of
    Open 1 -> do
      finishStmt
      put $ s { readState = Open 2 }
      stmt src
    Open   _ -> fail "Nest '{{}}' is not supported now."
    InOpen _ -> fail "Nest '{{}}' is not supported now."
    Out      -> do
      put $ s { readState = Open 1 }
      stmt src
    In -> do
      put $ s { readState = InOpen 1 }
      stmt src

consumeCloseBrace :: String -> State StmtState [String]
consumeCloseBrace src = do
  s <- get
  case readState s of
    In -> do
      put $ s { readState = Close 1 }
      stmt src
    Close 1 -> do
      put $ s { readState = Close 2 }
      finishStmt
      stmt src
    Close _ -> fail "Nest '{{}}' is not supported now."

consumeChar :: String -> Char -> State StmtState ()
consumeChar src next = do
  s <- get
  let top = getTop s
      rs  = case readState s of
        Open 2 -> In
        In     -> In
        _      -> Out
  put $ s { getTop = top ++ [next], readState = rs }
