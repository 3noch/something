{-# LANGUAGE CPP #-}

module Frontend.Selection where

import Control.Exception (SomeException)
import Control.Lens.Operators ((^.))
import Control.Monad.Catch (catch)
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.DOM.EventTargetClosures (unsafeEventName)
import qualified GHCJS.DOM as Dom
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Selection as Selection
import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.Window as Window
import Language.Javascript.JSaddle.Object (js)
import Reflex.Dom.Core (
    Event, Prerender,
    ffor, mapMaybe, never, performEvent, prerender, switchDyn, wrapDomEvent,
#   if !defined(ghcjs_HOST_OS)
      delay
#   endif
  )

selectionStart :: (Prerender js t m, Applicative m) => m (Event t (Text, Int, Text, Int))
selectionStart = fmap switchDyn $ prerender (pure never) $ do
  window <- Dom.currentWindowUnchecked
  selectStarted' <- wrapDomEvent window (`EventM.on` selectstart) (pure ())
  -- In JSaddle calling 'getSelection' must be delayed by a frame in order to get
  -- the most recent result. However it doesn't have this problem in native GHCJS.
#if defined(ghcjs_HOST_OS)
  let selectStarted = selectStarted'
#else
  selectStarted <- delay 0 selectStarted'
#endif
  fmap (mapMaybe id) $ performEvent $ ffor selectStarted $ \() -> Dom.liftJSM $ do
    sel <- Window.getSelectionUnchecked window
    let liftA4 f a b c d = f <$> a <*> b <*> c <*> d
    (liftA4.liftA4) (,,,)
      (getStartData =<< Selection.getAnchorNode sel)
      (Just . fromIntegral <$> Selection.getAnchorOffset sel)
      (getStartData =<< Selection.getExtentNode sel)
      (Just . fromIntegral <$> Selection.getExtentOffset sel)

  where
    selectstart :: EventM.EventName self Dom.Event
    selectstart = unsafeEventName (Dom.toJSString (s_ "selectstart"))

    getStartData = \case
      Nothing -> pure Nothing
      Just node -> (do
          parentNode <- Node.getParentNodeUnchecked node
          -- NOTE: This roundabout way of getting to 'Text' fixes a weird
          -- bug that only appears in GHCJS builds. Without this, an 'undefined'
          -- would somehow sneak past masquerading as 'Text' only to blow up
          -- when we use something like 'Data.Text.splitOn' later.
          x' <- Dom.fromJSVal @String =<< Node.unNode parentNode ^. js (s_ "dataset") . js (s_ "start")
          pure $ case x' of
            Nothing -> Nothing
            Just "" -> Nothing -- Does forcing the 'String' thunk here fix it the issue or is it just by using 'String' instead of 'Text'?
            Just x -> Just $ T.pack x
        ) `catch` \(_ :: SomeException) -> pure Nothing

    s_ :: String -> String = id
