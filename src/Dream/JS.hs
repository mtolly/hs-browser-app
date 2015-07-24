{-# LANGUAGE LambdaCase #-}
module Dream.JS where

import GHCJS.Types
import GHCJS.Foreign

type JS a = IO (JSRef a)

foreign import javascript unsafe "'' + $1"
  js_showJSRef :: JSRef a -> IO JSString

showJSRef :: JSRef a -> IO String
showJSRef = fmap fromJSString . js_showJSRef

prop :: JS a -> String -> JS b
prop obj k = obj >>= \o -> getPropMaybe k o >>= \case
  Nothing -> do
    s <- showJSRef o
    error $ "prop: " ++ s ++ " does not have property " ++ k
  Just v  -> return v

foreign import javascript unsafe "$1[$2].apply($1, $3)"
  js_method :: JSRef a -> JSString -> JSArray b -> IO (JSRef c)

call :: JS a -> String -> [JS b] -> JS c
call obj k args = do
  obj_  <- obj
  args_ <- sequence args
  arr <- toArray args_
  js_method obj_ (toJSString k) arr

foreign import javascript unsafe "window[$1]"
  js_window :: JSString -> IO (JSRef a)

window :: String -> JS a
window = js_window . toJSString

jq :: JS a
jq = window "$"
