{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Dream.JS where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import Control.Monad.IO.Class
import qualified Data.Text as T

-- | Untyped JS value
type JS = JSRef ()

foreign import javascript unsafe "'' + $1"
  js_showJSRef :: JSRef a -> IO JSString

showJSRef :: (MonadIO m) => JSRef a -> m T.Text
showJSRef = liftIO . fmap fromJSString . js_showJSRef

(.!) :: (MonadIO m) => m JS -> T.Text -> m JS
(.!) obj k = obj >>= \o -> liftIO (getPropMaybe k o) >>= \case
  Nothing -> do
    s <- showJSRef o
    error $ "(.!): " ++ T.unpack s ++ " does not have property " ++ T.unpack k
  Just v  -> return v
infixl 9 .!

foreign import javascript unsafe "$1[$2].apply($1, $3)"
  js_method :: JSRef a -> JSString -> JSArray b -> IO (JSRef c)

call :: (MonadIO m) => m JS -> T.Text -> [m JS] -> m JS
call obj k args = do
  obj_  <- obj
  args_ <- sequence args
  arr <- liftIO $ toArray args_
  liftIO $ js_method obj_ (toJSString k) arr

foreign import javascript unsafe "window[$1]"
  js_window :: JSString -> IO (JSRef a)

window :: (MonadIO m) => T.Text -> m JS
window = liftIO . js_window . toJSString

jq :: (MonadIO m) => m JS
jq = window "$"

toJS :: (MonadIO m, ToJSRef a) => a -> m JS
toJS = liftIO . fmap castRef . toJSRef

fromJS :: (MonadIO m, FromJSRef a) => m JS -> m a
fromJS js = do
  ref <- js
  res <- liftIO $ fromJSRef $ castRef ref
  case res of
    Nothing -> do
      s <- showJSRef ref
      error $ "fromJS: unmarshalling " ++ T.unpack s ++ " failed"
    Just v  -> return v
