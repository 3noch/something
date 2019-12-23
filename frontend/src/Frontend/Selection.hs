{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}

module Frontend.Selection where

import Control.Lens.Operators ((^.))
import Control.Monad.Trans (lift)
import Data.Text (Text)
import GHCJS.DOM.EventTargetClosures (unsafeEventName)
import qualified GHCJS.DOM as Dom
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.Types as Dom
import Language.Javascript.JSaddle (isNull, ghcjsPure)
import Language.Javascript.JSaddle.Object (js0, js)
import Reflex.Dom.Core (Prerender, Event, switchDyn, prerender, never, wrapDomEventMaybe)

import Control.Exception (SomeException)
import Control.Monad.Catch (catch)

selectionStart :: (Prerender js t m, Monad m) => m (Event t (Text, Int, Text, Int))
selectionStart = fmap switchDyn $ prerender (pure never) $ do
  window <- Dom.currentWindowUnchecked
  wrapDomEventMaybe window (`EventM.on` selectstart) $ lift $ do
    sel <- window ^. js0 (s_ "getSelection")
    let liftA4 f a b c d = f <$> a <*> b <*> c <*> d
    (liftA4.liftA4) (,,,)
      (getStartData "anchorNode" sel)
      (Dom.fromJSVal @Int =<< sel ^. js (s_ "anchorOffset"))
      (getStartData "extentNode" sel)
      (Dom.fromJSVal @Int =<< sel ^. js (s_ "extentOffset"))

  where
    selectstart :: EventM.EventName self Dom.Event
    selectstart = unsafeEventName (Dom.toJSString (s_ "selectstart"))

    getStartData nodeName sel = do
      node' <- sel ^. js (s_ nodeName)
      nodeIsNull <- ghcjsPure (isNull node')
      case nodeIsNull of
        True -> pure Nothing
        False -> (do
            raw <- node' ^. js (s_ "parentNode") . js (s_ "dataset") . js (s_ "start")
            Dom.fromJSVal @Text raw
          ) `catch` \(_ :: SomeException) -> pure Nothing




    s_ :: String -> String = id
