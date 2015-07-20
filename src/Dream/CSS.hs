{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Dream.CSS where

import Dream.HTML
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import qualified Data.Text as T
import Control.Monad.Fix
import Data.Monoid ((<>))
import Data.Char (isSpace)

newtype CSST m a = CSST
  { unCSST :: ReaderT (Element, T.Text) m a
  } deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)

addStyle :: (MonadIO m) => CSST m a -> m a
addStyle css = runHTMLT headElement $ do
  "style" </ do
    "type" $= "text/css"
    HTMLT $ withReaderT (, "") $ unCSST css

styleElement :: (Monad m) => CSST m Element
styleElement = CSST $ asks fst

runStyle :: HTMLT m a -> CSST m a
runStyle = CSST . withReaderT fst . unHTMLT

sel :: T.Text -> CSST m a -> CSST m a
sel s = CSST . withReaderT f . unCSST
  where f (elt, sels) = (elt, sels <> " " <> s)

($:) :: (MonadIO m) => T.Text -> T.Text -> CSST m ()
k $: v = do
  sels <- CSST $ asks snd
  let sels' = if T.all isSpace sels then "body" else sels
  runStyle $ html $ "\n" <> sels' <> " { " <> k <> ": " <> v <> " }"
infixr 0 $:
