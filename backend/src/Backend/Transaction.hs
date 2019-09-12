module Backend.Transaction where

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Catch (MonadThrow)
import Data.Pool (Pool, withResource)
import Database.Beam.Postgres (Pg, runBeamPostgresDebug)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Transaction as Pg
import Common.Prelude

data Writing

newtype Transaction mode a = Transaction { unTransaction :: ReaderT Pg.Connection IO a }
  deriving (Functor, Applicative, Monad, MonadThrow)

runTransaction :: MonadIO m => Pool Pg.Connection -> Transaction mode a -> m a
runTransaction dbPool (Transaction (ReaderT act)) = liftIO $ withResource dbPool $ \conn ->
  Pg.withTransactionSerializable conn $ act conn

runQuery :: Pg a -> Transaction mode a
runQuery act = Transaction $ ReaderT $ \conn -> runBeamPostgresDebug putStrLn conn act
