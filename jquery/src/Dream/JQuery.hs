module Dream.JQuery where

import GHCJS.Types

foreign import javascript unsafe "$" jQuery :: JSRef a
