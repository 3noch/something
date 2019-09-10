module Backend.Transaction where

import Data.Pool (Pool, withResource)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Transaction as Pg
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Catch (MonadThrow)

import Common.Prelude

newtype Transaction a = Transaction { unTransaction :: ReaderT Pg.Connection IO a }
  deriving (Functor, Applicative, Monad, MonadThrow)

runTransaction :: MonadIO m => Pool Pg.Connection -> Transaction a -> m a
runTransaction dbPool (Transaction (ReaderT act)) = liftIO $ withResource dbPool $ \conn ->
  Pg.withTransactionSerializable conn $ act conn
