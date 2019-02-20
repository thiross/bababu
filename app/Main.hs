{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Bababu.Parse
import           Bababu.Render
import qualified Data.ByteString.Lazy          as LBS
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
      contents <- LBS.readFile $ head args
      case parse contents of
        Left  msg -> hPutStrLn stderr msg
        Right n   -> LBS.hPut stdout (render "pages/index" n)
