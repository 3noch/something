{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Lens (ix, (^?), (^.))
import qualified Data.IntervalSet as IntervalSet
import Data.IntervalMap.Interval (Interval (..))
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup (First (..))
import qualified Data.Text as T
import Control.Monad.Fix (MonadFix)
import Obelisk.Configs (HasConfigs)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Rhyolite.Api (ApiRequest, public)
import Rhyolite.Frontend.App (RhyoliteWidget, functorToWire, runObeliskRhyoliteWidget, watchViewSelector)

import Obelisk.Generated.Static

import Common.App
import Common.Route
import Common.Prelude
import Common.Schema
import Data.Bible


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = headSection
  , _frontend_body = runAppWidget $ skeleton $ subRoute_ $ \case
      FrontendRoute_Main -> appWidget (pure (Canon_Old Genesis, Nothing))
      FrontendRoute_Reference -> appWidget =<< askRoute
  }

headSection :: DomBuilder t m => m ()
headSection = do
  elAttr "meta" ("charset"=:"utf-8") blank
  elAttr "meta" ("name"=:"viewport" <> "content"=:"width=device-width, initial-scale=1") blank
  elAttr "link" ("rel"=:"stylesheet" <> "type"=:"text/css" <> "href"=: static @"css/bulma.css") blank
  el "title" $ text "Something"
  elAttr "script" ("defer"=:"defer"<> "src"=:"https://use.fontawesome.com/releases/v5.3.1/js/all.js") blank

runAppWidget ::
  ( HasConfigs m
  , TriggerEvent t m
  , PerformEvent t m
  , MonadHold t m
  , PostBuild t m
  , MonadFix m
  , Prerender x t m
  )
  => RoutedT t (R FrontendRoute) (RhyoliteWidget (ViewSelector SelectedCount) (ApiRequest () PublicRequest PrivateRequest) t m) a
  -> RoutedT t (R FrontendRoute) m a
runAppWidget = runObeliskRhyoliteWidget
  functorToWire
  "common/route"
  checkedFullRouteEncoder
  (BackendRoute_Listen :/ ())


skeleton
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, SetRoute t (R FrontendRoute) m)
  => m a -> m a
skeleton body = do
  navBar
  a <- elClass "section" "section" body
  footer
  pure a


navBar
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, SetRoute t (R FrontendRoute) m)
  => m ()
navBar =
  elAttr "nav" ("class"=:"navbar" <> "role"=:"navigation" <> "aria-label"=:"main navigation") $ do
    divClass "navbar-brand" $
      elAttr "a" ("class"=:"navbar-item" <> "href"=:"https://bulma.io") $ text "Tree of Life"
        -- elAttr "img" ("src"=:"https://bulma.io/images/bulma-logo.png" <> "width"=:"112" <> "height"=:"28") blank

    (burgerEl, ()) <- elAttr' "a" ("role"=:"button" <> "class"=:"navbar-burger burger" <> "aria-label"=:"menu") $ do
      let ln = elAttr "span" ("aria-hidden"=:"true") blank
      ln *> ln *> ln

    menuIsActive <- toggle False $ domEvent Click burgerEl

    elDynAttr "div" (ffor menuIsActive $ \active -> "class"=:("navbar-menu" <> if active then " is-active" else "")) $ do
      divClass "navbar-start" $ do
        elClass "a" "navbar-item" $ text "Home"
        elClass "a" "navbar-item" $ text "Documentation"
        divClass "navbar-item has-dropdown is-hoverable" $ do
          elClass "a" "navbar-link" $ text "More"

          divClass "navbar-dropdown" $ do
            elClass "a" "navbar-item" $ text "About"
            elClass "a" "navbar-item" $ text "Jobs"
            elClass "a" "navbar-item" $ text "Contact"

            elClass "hr" "navbar-divider" blank
            elClass "a" "navbar-item" $ text "Report an issue"
        divClass "navbar-item" $
          divClass "field has-addons" $ do
            (refValue, enterPressed) <- elClass "p" "control" $ do
              inputEl <- inputElement $ def & initialAttributes .~ ("class"=:"input" <> "type"=:"text" <> "placeholder"=:"Go to reference")
              pure (value inputEl, keypress Enter $ _inputElement_element inputEl)

            goClicked <- elClass "p" "control" $ do
              (buttonEl, ()) <- elAttr' "button" ("class"=:"button" <> "type"=:"button") $
                text "Go"
              pure $ domEvent Click buttonEl

            let goToValidRef =
                  mapMaybe (either (const Nothing) Just . parseBibleReference) $ current refValue
                  <@ leftmost [goClicked, enterPressed]
            setRoute $ (FrontendRoute_Reference :/) <$> goToValidRef

      divClass "navbar-end" $
        divClass "navbar-item" $
          divClass "buttons" $ do
            elClass "a" "button is-primary" $
              el "strong" $ text "Sign up"
            elClass "a" "button is-light" $ text "Log in"

footer :: DomBuilder t m => m ()
footer =
  elClass "footer" "footer" $
    divClass "content has-text-centered" $
      el "p" $ do
        el "strong" (text "Tree of Life") *> text " by Roy Almasy and Elliot Cameron. The source code is licensed "
        elAttr "a" ("href"=:"http://opensource.org/licenses/mit-license.php") (text "MIT")
        text ". The website content is licensed " *> elAttr "a" ("href"=:"http://creativecommons.org/licenses/by-nc-sa/4.0/") (text "CC BY NC SA 4.0")

type HasApp t m =
  ( MonadQuery t (ViewSelector SelectedCount) m
  , Requester t m
  , Request m ~ ApiRequest () PublicRequest PrivateRequest
  , Response m ~ Identity
  )

defaultTranslation :: TranslationId
defaultTranslation = TranslationId 1

appWidget
  :: forall m t. (HasApp t m, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t (Canon, Maybe (Int, Maybe Int))
  -> m ()
appWidget referenceDyn = do
  now <- getPostBuild

  upClicked <- fmap (domEvent Click . fst) $ el' "div" $ text "Up"
  rec
    versesWidget <=< watchVerses $ (defaultTranslation, ) <$> range
    downClicked <- fmap (domEvent Click . fst) $ el' "div" $ text "Down"
    routeRangeDyn <- holdUniqDyn $ referenceToInterval . referenceToVerseReference <$> referenceDyn
    range <- foldDyn ($) (IntervalCO (VerseReferenceT (BookId 1) 1 1) (VerseReferenceT (BookId 1) 3 9999)) $ leftmost
     [ const <$> current routeRangeDyn <@ now
     , const <$> updated routeRangeDyn
     , upClicked $> \(IntervalCO (VerseReferenceT book1 chap1 _) _) ->
         IntervalCO
           (if chap1 == 1
             then VerseReferenceT (BookId $ max 1 (unBookId book1 - 1)) 1 1
             else VerseReferenceT book1 (chap1 - 1) 1)
           (VerseReferenceT book1 (chap1 + 3) 9999)
     , downClicked $> \(IntervalCO (VerseReferenceT book1 chap1 _) _) ->
         IntervalCO (VerseReferenceT book1 (chap1 + 1) 1) (VerseReferenceT book1 (chap1 + 4) 9999)
     ]

  pure ()

  where
    referenceToVerseReference :: (Canon, Maybe (Int, Maybe Int)) -> VerseReference
    referenceToVerseReference (b, verseChap) = VerseReferenceT (BookId $ fromEnum b + 1) chap verse
      where
        chap = maybe 1 fst verseChap
        verse = fromMaybe 1 $ snd =<< verseChap

    referenceToInterval :: VerseReference -> Interval VerseReference
    referenceToInterval (VerseReferenceT (BookId b) c _) = IntervalCO
      (VerseReferenceT (BookId b) (max 1 $ c - 1) 1)
      (VerseReferenceT (BookId b) (c + 1) 9999)

    versesWidget (verses, tags) = mdo
      (_, selectWord) <- fmap (second (fmap getFirst)) $ runEventWriterT $
        listWithKey (fromMaybe mempty . coerce <$> verses) $ \vref vDyn ->
          el "p" $ do
            let yellow = "style" =: "background-color: yellow;"
            let blue = "style" =: "background-color: blue;"
            text $ showVerseReference vref
            vDynUniq <- holdUniqDyn vDyn
            dyn_ $ ffor vDynUniq $ \v -> ifor_ (T.words v) $ \i w ->
              elDynAttr "span" (fromUniqDynamic $ ffor selections $ \sels -> if null (IntervalSet.containing sels (vref, i)) then mempty else yellow) $ do
                (elmnt, _) <- elDynAttr' "a" (ffor highlightState $ \firstWord -> if firstWord == Just (vref, i) then blue else mempty) $ text w
                text " "
                tellEvent $ First (vref, i) <$ domEvent Click elmnt

      let
        -- Given a new value @a@ and a previous 'Maybe', flip 'Just _' to 'Nothing' and 'Nothing' to @Just a@.
        toggleMaybes :: a -> Maybe a -> Maybe a
        toggleMaybes a = \case
          Nothing -> Just a
          Just _ -> Nothing

      -- Highlight state enters "highlighting" when you select your first word, then goes back when you select the second.
      -- Thus this state always stores the first word of a highlight or 'Nothing' if not highlighting.
      highlightState <- foldDyn toggleMaybes Nothing selectWord

      let
        captureRange :: Maybe (VerseReference, Int) -> (VerseReference, Int) -> Maybe (Interval (VerseReference, Int))
        captureRange firstWord lastWord = firstWord <&> \fstWord -> if lastWord >= fstWord
          then ClosedInterval fstWord lastWord
          else ClosedInterval lastWord fstWord

        highlightFinished :: Event t (Interval (VerseReference, Int)) = fmapMaybe id $ captureRange <$> current highlightState <@> selectWord

      let
        tagSpanToInterval (ref1, word1, ref2, word2) = ClosedInterval (ref1, word1) (ref2, word2)
        selections :: UniqDynamic t (IntervalSet.IntervalSet (Interval (VerseReference, Int))) =
          maybe mempty (IntervalSet.fromList . map tagSpanToInterval . toList . fold . MMap.elems) <$> uniqDynamic tags
      _ <- requestingIdentity $ ffor highlightFinished $ \(ClosedInterval start end) -> -- TODO: Partial match
        public $ PublicRequest_AddTag $ TagOccurrence "test" defaultTranslation start end
      pure ()

watchTranslations
  :: (HasApp t m, MonadHold t m, MonadFix m)
  => m (Dynamic t (Maybe (MonoidalMap TranslationId Translation)))
watchTranslations =
  (fmap . fmap) (fmap (fmap getFirst . snd) . getOption . _view_translations) $
    watchViewSelector $ pure $ mempty { _viewSelector_translations = Option $ Just 1 }

watchVerses
  :: (HasApp t m, MonadHold t m, MonadFix m)
  => Dynamic t (TranslationId, Interval VerseReference)
  -> m ( Dynamic t (Maybe (MonoidalMap VerseReference Text))
       , Dynamic t (Maybe (MonoidalMap Text (Set (VerseReference, Int, VerseReference, Int)))))
watchVerses rng = do
  result <- watchViewSelector $ ffor rng $ \(translationId, interval) -> mempty
    { _viewSelector_verseRanges = MMap.singleton translationId $ MMap.singleton interval 1 }
  pure $ splitDynPure $ ffor2 rng result $ \(translationId, _interval) result' -> -- TODO: Filter out verses that fit interval?
    ( (fmap.fmap) (getFirst . snd) $ result' ^? view_verses . ix translationId
    , (fmap.fmap) MMap.keysSet $ result' ^? view_tags . ix translationId
    )
