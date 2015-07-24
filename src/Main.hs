{-# LANGUAGE OverloadedStrings #-}
module Main where

import Dream.HTML
import Dream.CSS
import Dream.JS
import Data.Monoid ((<>))

main :: IO ()
main = do
  runHTMLT body $ do
    "p" </ do
      version <- fromJS $ jq .! "fn" .! "jquery"
      html $ "jQuery version " <> version
  addStyle $ do
    sel "body" $ do
      "color"     $: "red"
      "font-size" $: "20px"
