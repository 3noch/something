{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Backend.Schema where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (..))
import qualified Data.Aeson as Json
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate
import qualified Database.PostgreSQL.Simple as Pg
import qualified Gargoyle.PostgreSQL.Connect as Gargoyle
import Rhyolite.Backend.Listen (NotificationType (..), DbNotification (..), notifyChannel)
import Rhyolite.Schema (SchemaName (..))

import Backend.Transaction (Transaction (..))
import Common.App (ClosedInterval', TagOccurrence)
import Common.Prelude
import Common.Schema

data Db f = Db
  { _dbTranslation :: f (TableEntity TranslationT)
  , _dbBook :: f (TableEntity BookT)
  , _dbVerse :: f (TableEntity VerseT)
  , _dbTag :: f (TableEntity TagT)
  , _dbTaggedRange :: f (TableEntity TaggedRangeT)
  , _dbTaggedRangeByWord :: f (TableEntity TaggedRangeByWordT)
  , _dbTaggedRangeNote :: f (TableEntity TaggedRangeNoteT)
  }
  deriving stock Generic
  deriving anyclass (Database be)

db :: DatabaseSettings be Db
db = defaultDbSettings

checkedPgDb :: CheckedDatabaseSettings Postgres Db
checkedPgDb = defaultMigratableDbSettings

withDb :: MonadIO m => (Pool Pg.Connection -> IO a) -> m a
withDb f = liftIO $
  Gargoyle.withDb "db" $ \pool -> do
    withResource pool $ \conn ->
      runBeamPostgres conn $
        autoMigrate migrationBackend checkedPgDb
    f pool


data Notification a where
  Notification_Tag :: Notification (Presence, TagOccurrence)
  Notification_SetNotes :: Notification ((Text, ClosedInterval' (VerseReference, Int)), TaggedRangeNoteId)
deriving instance Show (Notification a)
fmap concat $ sequence
  [ deriveJSONGADT ''Notification
  , deriveArgDict ''Notification
  , deriveGShow ''Notification
  , deriveGEq ''Notification
  , deriveGCompare ''Notification
  ]


notify :: Notification a -> a -> Transaction mode ()
notify n a = void $ do
  let
    dontCare = ""
    cmd = "NOTIFY " <> fromString notifyChannel <> ", ?"
    notification = DbNotification { _dbNotification_schemaName = SchemaName dontCare
                                  , _dbNotification_notificationType = NotificationType_Update
                                  , _dbNotification_message = n :=> Identity a
                                  }

  Transaction $ ReaderT $ \conn ->
    Pg.execute conn cmd [T.unpack $ T.decodeUtf8 $ LBS.toStrict $ Json.encode notification]
