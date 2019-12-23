{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Lens (Prism', ix, prism', re)
import qualified Data.IntervalMap.Generic.Strict as IntervalMap
import Data.IntervalMap.Generic.Strict (IntervalMap)
import qualified Data.IntervalSet as IntervalSet
import Data.IntervalMap.Interval (Interval (..))
import Data.List (sortOn, foldl')
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup (First (..))
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.These (These (This, That, These))
import Control.Monad.Fix (MonadFix)
import Obelisk.Configs (HasConfigs)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Rhyolite.Api (ApiRequest, public)
import Rhyolite.Frontend.App (RhyoliteWidget, functorToWire, runObeliskRhyoliteWidget, watchViewSelector)
import Text.Read (readMaybe)

import Obelisk.Generated.Static

import Common.App
import Common.Route
import Common.Prelude
import Common.Schema
import Data.Bible

import Frontend.Selection (selectionStart)


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
  elAttr "link" ("rel"=:"stylesheet" <> "type"=:"text/css" <> "href"=:static @"css/bulma.css") blank
  el "title" $ text "Something"
  elAttr "script" ("defer"=:"defer" <> "src"=:"https://use.fontawesome.com/releases/v5.3.1/js/all.js") blank
  elAttr "script" ("defer"=:"defer" <> "src"=:static @"js/shims.js") blank

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

data IntervalEndpointType = Closed | Open deriving (Bounded, Enum, Eq, Generic, Ord, Show)

data IntervalEndpointPosition = Starting | Ending deriving (Bounded, Enum, Eq, Generic, Ord, Show)

data IntervalEndpoint a = IntervalEndpoint
  { _intervalEndpoint_value :: !a
  , _intervalEndpoint_type :: !IntervalEndpointType
  , _intervalEndpoint_position :: !IntervalEndpointPosition
  }
  deriving (Eq, Generic, Show)
instance Ord a => Ord (IntervalEndpoint a) where
  compare (IntervalEndpoint a' aType _) (IntervalEndpoint b' bType _) = case ((a', aType), (b', bType)) of
    ((a, Closed), (b, Open)) | a == b -> LT
    ((a, Open), (b, Closed)) | a == b -> GT
    ((a, _), (b, _)) -> compare a b

intervalToEndpoints :: Interval a -> (IntervalEndpoint a, IntervalEndpoint a)
intervalToEndpoints = \case
  IntervalCO a b -> (IntervalEndpoint a Closed Starting, IntervalEndpoint b Open Ending)
  IntervalOC a b -> (IntervalEndpoint a Open Starting, IntervalEndpoint b Closed Ending)
  ClosedInterval a b -> (IntervalEndpoint a Closed Starting, IntervalEndpoint b Closed Ending)
  OpenInterval a b -> (IntervalEndpoint a Open Starting, IntervalEndpoint b Open Ending)

endpointsToInterval :: (IntervalEndpoint a, IntervalEndpoint a) -> Interval a
endpointsToInterval = \case
  (IntervalEndpoint a Closed _, IntervalEndpoint b Open _) -> IntervalCO a b
  (IntervalEndpoint a Open _, IntervalEndpoint b Closed _) -> IntervalOC a b
  (IntervalEndpoint a Closed _, IntervalEndpoint b Closed _) -> ClosedInterval a b
  (IntervalEndpoint a Open _, IntervalEndpoint b Open _) -> OpenInterval a b

nonOverlapping
  :: forall k v. (Ord k, Ord v)
  => ((IntervalEndpoint k, IntervalEndpoint k) -> Set v -> Bool)
  -> IntervalMap (Interval k) v
  -> IntervalMap (Interval k) (Set v)
nonOverlapping _ xs | null xs = mempty
nonOverlapping filt xs = IntervalMap.fromAscList $ map (first endpointsToInterval) $ filter (uncurry filt) $ toList $ snd $ foldl'
  (\(active, intervals) ((this, v), (next, _)) ->
    let
      open x = x{_intervalEndpoint_type = Open}
      active' = case _intervalEndpoint_position this of
        Starting -> Set.insert v active
        Ending -> Set.delete v active
      interval = case (_intervalEndpoint_position this, _intervalEndpoint_position next) of
        (Starting, Ending) -> (this, next)
        (Starting, Starting) -> (this, open next)
        (Ending, Ending) -> (open this, next)
        (Ending, Starting) -> (open this, open next)
    in (active', intervals Seq.:|> (interval, active'))
  )
  (mempty, mempty)
  (zip endpoints (tail endpoints))
  where
    endpoints :: [(IntervalEndpoint k, v)] = sortOn fst $ fold $ do
      (interval, v) <- IntervalMap.toList xs
      let (left, right) = intervalToEndpoints interval
      pure [(left, v), (right, v)]

verseWords :: Text -> Seq Text
verseWords = Seq.fromList . filter (not . T.null) . T.split (' '==)

type WordInterval = Interval (VerseReference, Int)

versesToRanges :: MonoidalMap VerseReference Text -> IntervalMap WordInterval ()
versesToRanges
  =   fmap verseWords
  >>> MMap.mapWithKey verseWordsToInterval
  >>> MMap.elems
  >>> map (, ())
  >>> IntervalMap.fromList
  where
    verseWordsToInterval :: VerseReference -> Seq Text -> Interval (VerseReference, Int)
    verseWordsToInterval vref ws = ClosedInterval (vref, 0) (vref, length ws - 1)

tagsToRanges :: MonoidalMap Text (Set (VerseReference, Int, VerseReference, Int)) -> IntervalMap WordInterval Text
tagsToRanges tagMap = IntervalMap.fromList
  [ (ClosedInterval (vref1, w1) (vref2, w2), t)
  | (t, ranges) <- MMap.toList tagMap
  , (vref1, w1, vref2, w2) <- toList ranges
  ]

verseWordMap :: MonoidalMap VerseReference Text -> IntervalMap WordInterval Text
verseWordMap
  =   fmap verseWords -- TODO: Factor this out as it's done in multiple places
  >>> MMap.mapWithKey (\vref ws -> [(ClosedInterval (vref, idx) (vref, idx), w) | (idx, w) <- zip [0..] (toList ws)])
  >>> MMap.elems
  >>> concat
  >>> IntervalMap.fromList


combineTagsAndVerses
  :: IntervalMap WordInterval ()
  -> IntervalMap WordInterval Text
  -> IntervalMap WordInterval (These Text ())
combineTagsAndVerses verses tags = IntervalMap.unionWith merge_ (This <$> tags) (That <$> verses)
  where
    merge_ (This a) (That b) = These a b
    merge_ _ _ = error "Impossible"

mkNonOverlappingDomSegments
  :: MonoidalMap VerseReference Text
  -> MonoidalMap Text (Set (VerseReference, Int, VerseReference, Int))
  -> IntervalMap WordInterval (Set (These Text ()))
mkNonOverlappingDomSegments verses tags = nonOverlapping filt $ combineTagsAndVerses (versesToRanges verses) (tagsToRanges tags)
  where
    -- We can drop any interval that ends with an open endpoint on a 0th word. That interval
    -- corresponds to the spans between verses and those will never contain any text.
    filt (_, IntervalEndpoint (_, 0) Open _) _ = False
    filt _ _ = True

appWidget
  :: forall m js t. (HasApp t m, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m)
  => Dynamic t (Canon, Maybe (Int, Maybe Int))
  -> m ()
appWidget referenceDyn = do
  now <- getPostBuild

  e <- selectionStart
  widgetHold_ (text "...") $ ffor e $ \e' -> text $ tshow e'

  rec
    (verses, tags) <- watchVerses $ (defaultTranslation, ) <$> range
    versesWidget verses tags
    routeRangeDyn <- holdUniqDyn $ referenceToInterval . referenceToVerseReference <$> referenceDyn
    range <- foldDyn ($) (IntervalCO (VerseReferenceT (BookId 1) 1 1) (VerseReferenceT (BookId 1) 3 9999)) $ leftmost
      [ const <$> current routeRangeDyn <@ now
      , const <$> updated routeRangeDyn
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

    versesWidget
      :: Dynamic t (Maybe (MonoidalMap VerseReference Text))
      -> Dynamic t (MonoidalMap Text (Set (VerseReference, Int, VerseReference, Int)))
      -> m ()
    versesWidget verses' tags' = mdo
      domSegments' <- maybeDyn $ (liftA2 . liftA2) (,) verses' (Just <$> tags')
      dyn_ $ ffor domSegments' $ \case
        Nothing -> text "Loading..."
        Just versesTags -> divClass "passage" $ do
          let
            (verses, tags) = splitDynPure versesTags
            wordMap = verseWordMap <$> verses
            domSegments = Map.fromDistinctAscList . IntervalMap.toAscList <$> liftA2 mkNonOverlappingDomSegments verses tags
          void $ listWithKey domSegments $ \k v -> do
            let startingEndpoint = fst (intervalToEndpoints k)
            case startingEndpoint of
              IntervalEndpoint (VerseReferenceT book chapter verse, 0) Closed _
                | chapter == 1 && verse == 1 -> el "br" blank *> text " " *> el "strong" (text $ tshow ((\(BookId n) -> n) book) <> ":1")
                | verse == 1 -> el "br" blank *> text " " *> el "strong" (text $ tshow chapter)
                | otherwise -> el "br" blank *> text " " *> el "sup" (text $ tshow verse)
              _ -> blank
            let
              wordsDyn = T.intercalate " " . map snd . IntervalMap.toAscList . (`IntervalMap.intersecting` k) <$> wordMap
              isHighlighted = any (\case This _ -> True; These _ _ -> True; That _ -> False) <$> v
              styleDyn = ffor isHighlighted $ \hl ->
                "data-start" =: startingEndpoint ^. re wordStartingEndpointEncoding <>
                if hl then "style" =: "background-color:yellow;" else mempty
            text " "
            elDynAttr "span" styleDyn $ dynTextStrict wordsDyn

wordStartingEndpointEncoding :: Prism' Text (IntervalEndpoint (VerseReference, Int))
wordStartingEndpointEncoding = prism'
  (\(IntervalEndpoint (VerseReferenceT book chapter verse, word) t _) ->
    T.intercalate ":" $ map (T.pack . show) [(\(BookId n) -> n) book, chapter, verse, case t of Closed -> word; Open -> word + 1]
  )
  (\txt -> case T.splitOn ":" txt of
    [book', chapter', verse', word']
      | Just book <- read' book'
      , Just chapter <- read' chapter'
      , Just verse <- read' verse'
      , Just word <- read' word'
      -> Just $ IntervalEndpoint (VerseReferenceT (BookId book) chapter verse, word) Closed Starting
      where read' = readMaybe . T.unpack
    _ -> Nothing
  )

-- | A more efficient version of 'dynText' but with the added requirement that the input 'Dynamic' be strict.
-- That is, it must have a value at DOM-building time because it's initial value is immediately sampled.
dynTextStrict :: (DomBuilder t m, MonadSample t m) => Dynamic t Text -> m ()
dynTextStrict d = do
  d0 <- sample (current d)
  void $ textNode $ def & textNodeConfig_initialContents .~ d0 & textNodeConfig_setContents .~ updated d


      -- (_, selectWord) <- fmap (second (fmap getFirst)) $ runEventWriterT $
      --   listWithKey (fromMaybe mempty . coerce <$> verses') $ \vref vDyn ->
      --     el "p" $ do
      --       let yellow = "style" =: "background-color: yellow;"
      --       let blue = "style" =: "background-color: grey;"
      --       text $ T.pack (show $ _versereferenceChapter vref) <> ":" <> T.pack (show $ _versereferenceVerse vref) <> " "
      --       vDynUniq <- holdUniqDyn vDyn
      --       dyn_ $ ffor vDynUniq $ \v -> ifor_ (T.words v) $ \i w ->
      --         elDynAttr "span" (fromUniqDynamic $ ffor selections $ \sels -> if null (IntervalSet.containing sels (vref, i)) then mempty else yellow) $ do
      --           (elmnt, _) <- elDynAttr' "a" (ffor highlightState $ \firstWord -> if firstWord == Just (vref, i) then blue else mempty) $ text w
      --           text " "
      --           tellEvent $ First (vref, i) <$ domEvent Click elmnt

      -- let
      --   -- Given a new value @a@ and a previous 'Maybe', flip 'Just _' to 'Nothing' and 'Nothing' to @Just a@.
      --   toggleMaybes :: a -> Maybe a -> Maybe a
      --   toggleMaybes a = \case
      --     Nothing -> Just a
      --     Just _ -> Nothing

      -- -- Highlight state enters "highlighting" when you select your first word, then goes back when you select the second.
      -- -- Thus this state always stores the first word of a highlight or 'Nothing' if not highlighting.
      -- highlightState <- foldDyn toggleMaybes Nothing selectWord

      -- let
      --   captureRange :: Maybe (VerseReference, Int) -> (VerseReference, Int) -> Maybe (Interval (VerseReference, Int))
      --   captureRange firstWord lastWord = firstWord <&> \fstWord -> if lastWord >= fstWord
      --     then ClosedInterval fstWord lastWord
      --     else ClosedInterval lastWord fstWord

      --   highlightFinished :: Event t (Interval (VerseReference, Int)) = fmapMaybe id $ captureRange <$> current highlightState <@> selectWord

      -- let
      --   tagSpanToInterval (ref1, word1, ref2, word2) = ClosedInterval (ref1, word1) (ref2, word2)
      --   selections :: UniqDynamic t (IntervalSet.IntervalSet (Interval (VerseReference, Int))) =
      --     maybe mempty (IntervalSet.fromList . map tagSpanToInterval . toList . fold . MMap.elems) <$> uniqDynamic tags'
      -- _ <- requestingIdentity $ ffor highlightFinished $ \(ClosedInterval start end) -> -- TODO: Partial match
      --   public $ PublicRequest_AddTag $ TagOccurrence "test" defaultTranslation start end
      --pure ()

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
       , Dynamic t (MonoidalMap Text (Set (VerseReference, Int, VerseReference, Int))))
watchVerses rng = do
  result <- watchViewSelector $ ffor rng $ \(translationId, interval) -> mempty
    { _viewSelector_verseRanges = MMap.singleton translationId $ MMap.singleton interval 1 }
  pure $ splitDynPure $ ffor2 rng result $ \(translationId, _interval) result' -> -- TODO: Filter out verses that fit interval?
    ( (fmap.fmap) (getFirst . snd) $ result' ^? view_verses . ix translationId
    , maybe mempty (fmap MMap.keysSet) $ result' ^? view_tags . ix translationId
    )
