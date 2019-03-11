{-# LANGUAGE OverloadedStrings #-}
module Bababu.Render
  ( render
  )
where

import           Bababu.Types
import           Control.Monad.Free
import           Control.Monad.State
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as LBS8
import           Data.List                      ( intercalate )
import           Debug.Trace

render :: ByteString -> Program ByteString r -> ByteString
render n p =
  LBS.concat
    $  [ "__mini_app_set_current_page_name(\""
       , n
       , "\");__mini_app_install_renderer(\""
       , n
       , "\",\"\",function(){with(this){return "
       ]
    ++ render' p
    ++ ["}});"]

render' :: Program ByteString r -> [ByteString]
render' (Free Done                 ) = []
render' (Free (Expression txt next)) = expr txt : render' next
render' (Free (Block t as cn next)) =
  LBS.concat
      [ "_h(\""
      , htmlTag t
      , "\",{class:{"
      , classes as
      , "},props:{"
      , LBS.intercalate ","
        $ map pair (filter (not . LBS.isPrefixOf "wx:" . fst) as)
      , "},on:{"
      , LBS.intercalate "," (renderHandler as)
      , "}},["
      , LBS.intercalate "," $ render' cn
      , "])"
      ]
    : render' next
 where
  pair (k, v) = LBS.concat ["\"", k, "\":", expr v]
  classes []                  = ""
  classes (("class", v) : _ ) = LBS.concat ["\"", v, "\":true"]
  classes (_            : as) = classes as
render' (Free (IfBlock t as ife ele next)) =
  LBS.concat
      [ "_h(\""
      , htmlTag t
      , "\",{class:{"
      , classes as
      , "},props:{"
      , LBS.intercalate ","
        $ map pair (filter (not . LBS.isPrefixOf "wx:" . fst) as)
      , "},on:{"
      , LBS.intercalate "," (renderHandler as)
      , "}},"
      , "("
      , cond as
      , ")?["
      , LBS.intercalate "," $ render' ife
      , "]:["
      , LBS.intercalate "," $ render' ele
      , "])"
      ]
    : render' next
 where
  cond (("wx:if", e) : _ ) = expr e
  cond (_            : as) = cond as
  pair (k, v) = LBS.concat ["\"", k, "\":", expr v]
  classes []                  = ""
  classes (("class", v) : _ ) = LBS.concat ["\"", v, "\":true"]
  classes (_            : as) = classes as

renderHandler :: [(ByteString, ByteString)] -> [ByteString]
renderHandler [] = []
renderHandler (("bindtap", h) : as) =
  LBS.concat ["click:", h] : renderHandler as
renderHandler (a : as) = renderHandler as

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

htmlTag :: ByteString -> ByteString
htmlTag t = case t of
  "view"  -> "div"
  "block" -> "div"
  "text"  -> "span"
  "image" -> "img"
  _       -> t

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
