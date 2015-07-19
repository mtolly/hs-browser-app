{-# LANGUAGE OverloadedStrings #-}
module Main where

import Dream.HTML
import Dream.JS
import GHCJS.Foreign
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))

main :: IO ()
main = runHTMLT body $ do
  replicateM_ 5 $ do
    "p" </ do
      version <- liftIO $ fromJSString <$> prop jq "jquery"
      html $ "jQuery version " <> version
