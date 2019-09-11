module Backend.ViewSelectorHandler where

import Backend.Transaction (Transaction)
import Common.App (View (..), ViewSelector (..))

viewSelectorHandler :: Semigroup a => (Transaction x -> IO x) -> ViewSelector a -> IO (View a)
viewSelectorHandler _runTransaction = \case
  ViewSelector{} -> pure $ View mempty mempty
