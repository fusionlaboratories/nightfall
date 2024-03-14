{-# LANGUAGE ScopedTypeVariables #-}

-- | A custom prelude. The name starts with an 'A', so that this module appears first in most cases
-- when module imports are sorted lexicographically.
module Nightfall.Alphabet
     ( module Export
     , module Nightfall.Alphabet
     ) where

-- base
import Control.Applicative as Export
import Control.Exception as Export hiding (TypeError)
import Control.Monad as Export
import Control.Monad.IO.Class as Export
import Data.Bifunctor as Export
import Data.Bool as Export
import Data.Char as Export
import Data.Coerce as Export
import Data.Complex as Export
import Data.Either as Export
import Data.Foldable as Export
import Data.Function as Export
import Data.Functor as Export
import Data.Functor.Compose as Export
import Data.Functor.Identity as Export
import Data.List as Export
import Data.Maybe as Export
import Data.Monoid as Export hiding (First (..), Last (..))
import Data.Ord as Export
import Data.Proxy as Export
import Data.Ratio as Export
import Data.Semigroup as Export
import Data.String as Export
import Data.Traversable as Export
import Data.Tuple as Export
import Data.Typeable as Export
import Data.Void as Export
import Data.Word as Export
import GHC.TypeLits as Export hiding (Mod)
import GHC.Bits as Export (toIntegralSized)
import GHC.Exts as Export hiding (Any, toList)
import GHC.Generics as Export (Generic, Generic1)
import GHC.Natural as Export
import GHC.Stack as Export
import Numeric as Export
import System.IO as Export
import Text.Read as Export (Read (..), readEither, readMaybe)

-- containers
import Data.IntMap.Strict as Export (IntMap)
import Data.IntSet as Export (IntSet)
import Data.Map.Strict as Export (Map)
import Data.Set as Export (Set)

-- text
import Data.Text as Export (Text)

-- lens
import Control.Lens as Export (makeLenses)

-- wide-word
import Data.WideWord.Word256 as Export

infixr 8 ^!
-- | Power with fixed exponent type. This eliminates warnings about using default types.
(^!) :: Integral a => a -> Int -> a
(^!) = (^)

-- | Applicatively fold a 'Foldable' with a function.
foldMapA
    :: forall b m f a. (Monoid b, Applicative m, Foldable f)
    => (a -> m b) -> f a -> m b
foldMapA = coerce (foldMap :: (a -> Ap m b) -> f a -> Ap m b)

-- | Applicatively fold a 'Foldable'.
foldA
    :: forall a m f. (Monoid a, Applicative m, Foldable f)
    => f (m a) -> m a
foldA = foldMapA id

-- | Applicatively fold a 'Foldable' with a function.
foldFor
    :: forall b m f a. (Monoid b, Applicative m, Foldable f)
    => f a -> (a -> m b) -> m b
foldFor = flip foldMapA
