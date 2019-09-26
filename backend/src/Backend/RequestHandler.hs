module Backend.RequestHandler where

import Database.Beam
import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam.Backend.SQL.BeamExtensions as Ext
import Rhyolite.Api (ApiRequest (..))
import Rhyolite.Backend.App (RequestHandler (..))

import Backend.Schema
import Backend.Transaction (Transaction, runQuery)
import Common.App (PrivateRequest (..), PublicRequest (..))
import Common.Schema

requestHandler :: (forall x. Transaction mode x -> m x) -> RequestHandler (ApiRequest () PublicRequest PrivateRequest) m
requestHandler runTransaction =
  RequestHandler $ runTransaction . \case
    ApiRequest_Public r -> case r of
      PublicRequest_AddTag tag translationId (startRef, startWord) (endRef, endWord) -> runQuery $ do
        -- TODO: Add real unique indexs to get rid of this dance and just rely on the "on conflict" logic.
        existingTagT <- runSelectReturningList $ select $ do
          filter_ (\t -> _tagName t ==. val_ tag) $ all_ (_dbTag db)

        [tagT] <- case existingTagT of
          [] -> Ext.runInsertReturningList $
                  Ext.insertOnConflict (_dbTag db) (insertExpressions [TagT { _tagId = default_, _tagName = val_ tag }])
                    (Ext.anyConflict @Pg.Postgres @TagT) (Ext.onConflictDoNothing @Pg.Postgres @TagT)
          [t] -> pure [t]
          _ -> error "More than one tag with the same name!"

        [taggedRangeT] <- Ext.runInsertReturningList $
          insert (_dbTaggedRange db) (insertExpressions
            [ TaggedRangeT
                { _taggedrangeId = default_
                , _taggedrangeFor = val_ $ pk tagT
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

        pure ()


    ApiRequest_Private _key r -> case r of
      PrivateRequest_NoOp -> return ()
