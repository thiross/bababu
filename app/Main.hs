module Main where

import           Bababu.Parse
import qualified Data.ByteString               as B
import           System.Environment
import           System.IO

usage :: IO ()
usage = hPutStrLn stderr "bababu FILENAME.(wxml|xml)"

main :: IO ()
main = do
  args <- getArgs
  if length args < 1
    then usage
    else do
      contents <- B.readFile $ head args
      case template contents of
        Left  msg -> hPutStrLn stderr msg
        Right _   -> usage

