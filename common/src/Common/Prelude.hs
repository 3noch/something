module Common.Prelude (module Common.Prelude, module X) where

import Control.Applicative as X (liftA2)
import Control.Category as X ((<<<), (>>>))
import Control.Lens as X ((.~), (<&>), (?~), (^.), (^?), _1, _2, _3, ifor, ifor_, itraverse, itraverse_, preview, set, view)
import Control.Monad as X ((<=<), (>=>), guard, join)
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Data.Aeson as X (FromJSON, ToJSON)
import Data.Bifunctor as X (first, second)
import Data.Coerce as X (Coercible, coerce)
import Data.Foldable as X (fold, foldl', for_, toList)
import Data.Function as X ((&))
import Data.Functor as X (($>), void)
import Data.Functor.Compose as X (Compose (..))
import Data.Functor.Identity as X (Identity (..))
import Data.List.NonEmpty as X (nonEmpty)
import Data.Map as X (Map)
import Data.Map.Monoidal as X (MonoidalMap)
import Data.Maybe as X (fromMaybe, listToMaybe)
import Data.Ord as X (comparing)
import Data.Semigroup as X (First (..), Option (..))
import Data.Set as X (Set)
import Data.Text as X (Text)
import qualified Data.Text as T
import Data.Time as X (UTCTime)
import Data.Traversable as X (for)
import Data.Witherable as X (Filterable (mapMaybe))
import GHC.Generics as X (Generic)

tshow :: Show a => a -> Text
tshow = T.pack . show
