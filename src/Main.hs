{-# LANGUAGE OverloadedStrings #-}
module Main where

import Dream.HTML
import Dream.CSS
import Dream.JS
import GHCJS.Foreign
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))

main :: IO ()
main = do
  runHTMLT body $ do
    "p" </ do
      version <- liftIO $ fromJSString <$> prop (prop jq "fn") "jquery"
      html $ "jQuery version " <> version
  addStyle $ do
    sel "body" $ do
      "color"     $: "red"
      "font-size" $: "20px"
