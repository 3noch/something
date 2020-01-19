module Backend.RequestHandler where

import Database.Beam
import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam.Backend.SQL.BeamExtensions as Ext
import qualified Data.Set as Set
import qualified Data.Time as Time
import Rhyolite.Api (ApiRequest (..))
import Rhyolite.Backend.App (RequestHandler (..))

import Backend.Schema
import Backend.Transaction (Transaction, runQuery)
import qualified Backend.ViewSelectorHandler as VSH
import Common.App (ClosedInterval' (..), PrivateRequest (..), PublicRequest (..), TagOccurrence (..))
import Common.Prelude
import Common.Schema

requestHandler :: (forall x. Transaction mode x -> m x) -> RequestHandler (ApiRequest () PublicRequest PrivateRequest) m
requestHandler runTransaction =
  RequestHandler $ runTransaction . \case
    ApiRequest_Public r -> case r of
      PublicRequest_AddTag occurrence@(TagOccurrence tag translationId (ClosedInterval' (startRef, startWord) (endRef, endWord))) -> do
        runQuery $ do
          -- TODO: Add real unique indexs to get rid of this dance and just rely on the "on conflict" logic.
          existingTagT <- runSelectReturningList $ select $ do
            filter_ (\t -> _tagName t ==. val_ tag) $ all_ (_dbTag db)

          [tagT] <- case existingTagT of
            [] -> Ext.runInsertReturningList $
                    Ext.insertOnConflict (_dbTag db) (insertExpressions [TagT { _tagId = default_, _tagName = val_ tag }])
                      (Ext.anyConflict @Pg.Postgres @TagT) (Ext.onConflictDoNothing @Pg.Postgres @TagT)
            [t] -> pure [t]
            _ -> error $ "More than one tag with the same name " <> show tag

          [taggedRangeT] <- Ext.runInsertReturningList $
            insert (_dbTaggedRange db) (insertExpressions
              [ TaggedRangeT
                  { _taggedrangeId = default_
                  , _taggedrangeForTag = val_ $ pk tagT
                  , _taggedrangeStart = val_ startRef
                  , _taggedrangeEnd = val_ endRef
                  }
              ])

          runInsert $
            insert (_dbTaggedRangeByWord db) (insertExpressions
              [ TaggedRangeByWordT
                  { _taggedrangebywordForRange = val_ $ pk taggedRangeT
                  , _taggedrangebywordForTranslation = val_ translationId
                  , _taggedrangebywordStart = val_ startWord
                  , _taggedrangebywordEnd = val_ endWord
                  }
              ])

        notify Notification_Tag (Present, occurrence)

      PublicRequest_DeleteTag occurrence@(TagOccurrence tagName translationId interval) -> do
        runQuery $ do
          rangeIds <- runSelectReturningList $ select $ do
            tagTables@(_, taggedRange, taggedRangeByWord) <- VSH.allTagsAndRelated
            VSH.guardExactTagRangeMatches (Set.singleton (tagName, interval)) tagTables
            guard_ (_taggedrangebywordForTranslation taggedRangeByWord ==. val_ translationId)
            pure $ _taggedrangeId taggedRange

          runDelete $ delete (_dbTaggedRangeByWord db) $ \t ->
            _taggedrangebywordForRange t `in_` map (val_ . coerce) rangeIds

          -- Garbage collect old tag ranges that no longer have any "by-word" highlights
          leftoverRangeByWords <- fmap listToMaybe $ runSelectReturningList $ select $ limit_ 1 $ do
            t <- all_ (_dbTaggedRangeByWord db)
            guard_ $ _taggedrangebywordForRange t `in_` map (val_ . coerce) rangeIds
            pure $ _taggedrangebywordForTranslation t

          case leftoverRangeByWords of
            Nothing -> do
              runDelete $ delete (_dbTaggedRangeNote db) $ \t ->
                _taggedrangenoteForRange t `in_` map (val_ . coerce) rangeIds
              runDelete $ delete (_dbTaggedRange db) $ \t ->
                pk t `in_` map (val_ . coerce) rangeIds
            _ -> pure ()

        notify Notification_Tag (Absent, occurrence)

      PublicRequest_SetNotes tag notes utc -> do
        let asLocalTime = Time.utcToLocalTime Time.utc utc
        tagRangeIdAndNote' <-
          fmap (atMostOne $ error $ "Got more than one tag range for " <> show tag) $
            runQuery $ runSelectReturningList $ select $ limit_ 2 $ do
              tagTables@(_, taggedRange, _) <- VSH.allTagsAndRelated
              VSH.guardExactTagRangeMatches (Set.singleton tag) tagTables
              note <- leftJoin_ (all_ $ _dbTaggedRangeNote db) (\x -> _taggedrangenoteForRange x `references_` taggedRange)
              pure (_taggedrangeId taggedRange, note)

        for_ tagRangeIdAndNote' $ \(tagRangeId, existingNote') -> do
          newNote <- fmap (fromMaybe (error "WAT") . listToMaybe) $ case existingNote' of
            Nothing -> runQuery $ Ext.runInsertReturningList $ insert (_dbTaggedRangeNote db) $ insertExpressions
              [ TaggedRangeNoteT
                  { _taggedrangenoteId = default_
                  , _taggedrangenoteForRange = val_ (TaggedRangeId tagRangeId)
                  , _taggedrangenoteContent = val_ notes
                  , _taggedrangenoteUpdated = val_ asLocalTime
                  }
              ]
            Just existing -> ([existing] <$) $ runQuery $ runUpdate $ update (_dbTaggedRangeNote db)
              (\x -> mconcat [ _taggedrangenoteContent x <-. val_ notes, _taggedrangenoteUpdated x <-. val_ asLocalTime ])
              (\x -> pk x ==. val_ (pk existing))
          notify Notification_SetNotes (tag, pk newNote)

    ApiRequest_Private _key r -> case r of
      PrivateRequest_NoOp -> return ()

atMostOne :: Maybe a -> [a] -> Maybe a
atMostOne defalt = \case
  [] -> Nothing
  [a] -> Just a
  _ -> defalt
