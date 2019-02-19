{-# LANGUAGE OverloadedStrings #-}
module Bababu.Render where

import           Bababu.Types
import           Control.Monad.Free
import           Control.Monad.State
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as LBS8
import           Data.List                      ( intercalate )

render :: Program ByteString r -> ByteString
render (Free Done                 ) = ""
render (Free (Expression txt next)) = if LBS.length o == 0
  then e
  else LBS.concat [e, ",", o]
 where
  e = expr txt
  o = render next
render (Free (Block t as cn next)) = LBS.concat
  [ "_h(\""
  , t
  , "\",{"
  , LBS.intercalate ","
    $ map pair (filter (not . LBS.isPrefixOf "wx:" . fst) as)
  , "},["
  , render cn
  , "])\n"
  , render next
  ]
  where pair (k, v) = LBS.concat ["\"", k, "\":", expr v]
render (Free (IfBlock t as ifs els next)) = "if"

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

expr :: ByteString -> ByteString
expr e =
  let (es, _) = runState (stmt $ LBS8.unpack e) initState
  in  LBS8.pack $ intercalate "+" es

initState =
  StmtState {readPos = 0, readState = Out, getStmts = [], getTop = ""}

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

data FinishType
  = Expr
  | Literal
  deriving Show

finishStmtBy :: FinishType -> String -> State StmtState [String]
finishStmtBy t extra = do
  s <- get
  let top               = getTop s
      (prefix, postfix) = case t of
        Literal -> if '"' `elem` top then ("'", "'") else ("\"", "\"")
        Expr    -> ("(", ")")
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
    Close 2 -> finishStmtBy Expr ""
    Out     -> finishStmtBy Literal ""
    Open 1  -> finishStmtBy Literal "{"
    Open _  -> fail "Expected '}}' not found."
    In      -> fail "Expected '}}' not found."

consumeOpenBrace :: String -> State StmtState [String]
consumeOpenBrace src = do
  s <- get
  case readState s of
    Open 1 -> do
      put $ s { readState = Out }
      finishStmt
      modify $ \s -> s { readState = Open 2 }
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
      n   = case next of
        '\r' -> "\\r"
        '\n' -> "\\n"
        _    -> [next]
      rs = case readState s of
        Open 2 -> In
        In     -> In
        _      -> Out
  put $ s { getTop = top ++ n, readState = rs }
