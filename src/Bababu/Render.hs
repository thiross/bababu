{-# LANGUAGE OverloadedStrings #-}
module Bababu.Render where

import           Bababu.Parse
import           Control.Monad.State
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as LC8

-- render :: [Node ByteString] -> ByteString
-- render nodes = LBS.concat (map renderNode nodes)

data ReadState
  = In
  | Out
  | InOpen Int
  | Open Int
  | Close Int

data StmtState = RenderState
  { readPos   :: Int
  , readState :: ReadState
  , getStmts  :: [String]
  , getTop    :: String
  }

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
        _   -> consumeChar src c

finishStmtBy :: String -> String -> String -> State StmtState [String]
finishStmtBy prefix postfix extra = do
  s <- get
  let top = getTop s
  if length top == 0 && length extra == 0
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
      put $ s { readState = Open 2 }
      finishStmt
      stmt src
    Open   _ -> fail "Nested '{{}}' is not supported now."
    InOpen _ -> fail "Nested '{{}}' is not supported now."
    In       -> do
      put $ s { readState = InOpen 1 }
      stmt src

consumeCloseBrace :: String -> State StmtState [String]
consumeCloseBrace src = do
  s <- get
  case readState s of
    In -> do
      put $ s { readState = Close 1 }
      stmt src
    Close _ -> fail "Nested '{{}}' is not supported now."

