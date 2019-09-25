{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Backend.Schema where

import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate
import qualified Gargoyle.PostgreSQL.Connect as Gargoyle
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool, withResource)
import qualified Database.PostgreSQL.Simple as Pg

import Common.Schema

data Db f = Db
  { _dbTranslation :: f (TableEntity TranslationT)
  , _dbBook :: f (TableEntity BookT)
  , _dbVerse :: f (TableEntity VerseT)
  , _dbTag :: f (TableEntity TagT)
  , _dbTaggedRange :: f (TableEntity TaggedRangeT)
  , _dbTaggedRangeByWord :: f (TableEntity TaggedRangeByWordT)
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
