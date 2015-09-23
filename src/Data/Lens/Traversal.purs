-- | This module defines functions for working with traversals.

module Data.Lens.Traversal
  ( traversed
  , traverseOf
  , sequenceOf
  , failover
  ) where

import Prelude

import Data.Const
import Data.Monoid
import Data.Monoid.Disj
import Data.Tuple
import Data.Lens.Types
import Data.Profunctor.Star
import Control.Alternative
import Control.Plus
import Data.Traversable (Traversable, traverse)
import Data.Lens.Internal.Wander (wander)

-- | Create a `Traversal` which traverses the elements of a `Traversable` functor.
traversed :: forall t a b. (Traversable t) => Traversal (t a) (t b) a b
traversed = wander traverse

-- | Turn a pure profunctor `Traversal` into a `lens`-like `Traversal`.
traverseOf
  :: forall f s t a b. (Applicative f)
  => Optic (Star f) s t a b -> (a -> f b) -> s -> f t
traverseOf t = runStar <<< t <<< Star

-- | Sequence the foci of a `Traversal`, pulling out an `Applicative` effect.
-- | If you do not need the result, see `sequenceOf_` for `Fold`s.
sequenceOf
  :: forall f s t a. (Applicative f)
  => Optic (Star f) s t (f a) a -> s -> f t
sequenceOf t = traverseOf t id

-- | Tries to map over a `Traversal`; returns `empty`, if the traversal did
-- | not have any new focus.
failover
  :: forall f s t a b. (Alternative f)
  => Optic (Star (Tuple (Disj Boolean))) s t a b -> (a -> b) -> s -> f t
failover t f s = case runStar (t $ Star $ Tuple (Disj true) <<< f) s of
  Tuple (Disj true) x  -> pure x
  Tuple (Disj false) _ -> empty
