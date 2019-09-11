module Backend.ViewSelectorHandler where

import Data.Semigroup (First (..))
import Database.Beam

import Backend.Schema
import Backend.Transaction (Transaction, runQuery)
import Common.App (View (..), ViewSelector (..))
import Common.Prelude
import qualified Data.Map.Monoidal as MMap

viewSelectorHandler :: (Eq a, Semigroup a) => (forall x. Transaction x -> IO x) -> ViewSelector a -> IO (View a)
viewSelectorHandler runTransaction vs = if vs == mempty then pure mempty else runTransaction $ do
  translations <- fmap Option $ case getOption $ _viewSelector_translations vs of
    Nothing -> pure Nothing
    Just a -> do
      translations <- runQuery $ runSelectReturningList $ select $ all_ (_dbTranslation db)
      pure $ Just (a, MMap.fromList [(pk t, First t) | t <- translations])

  pure $ View translations mempty
