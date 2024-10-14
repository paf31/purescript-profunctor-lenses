module Test.Main where

import Prelude

import Control.Monad.State (evalState, get)
import Data.Either (Either(..))
import Data.Lens (Getter', Prism', _1, _2, _Just, _Left, collectOf, lens, lens', lensStore, preview, prism', takeBoth, toArrayOf, traversed, view)
import Data.Lens.Fold ((^?))
import Data.Lens.Fold.Partial ((^?!), (^@?!))
import Data.Lens.Grate (Grate, cloneGrate, grate, zipWithOf)
import Data.Lens.Index (ix)
import Data.Lens.Indexed (itraversed, reindexed)
import Data.Lens.Lens (IndexedLens, cloneIndexedLens, ilens)
import Data.Lens.Record (prop)
import Data.Lens.Setter (iover)
import Data.Lens.Traversal (cloneTraversal)
import Data.Lens.Zoom (ATraversal', IndexedTraversal', Lens, Lens', Traversal, Traversal', zoom)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assertEqual')
import Type.Proxy (Proxy(..))

foo :: forall a b r. Lens { foo :: a | r } { foo :: b | r } a b
foo = prop (Proxy :: Proxy "foo")

bar :: forall a b r. Lens { bar :: a | r } { bar :: b | r } a b
bar = prop (Proxy :: Proxy "bar")

barAndFoo :: forall a b r. Getter' { bar :: a, foo :: b | r } (Tuple a b)
barAndFoo = takeBoth bar foo

fooGetter :: forall x. Getter' { foo :: x } x
fooGetter = foo

barGetter :: forall x. Getter' { bar :: x } x
barGetter = bar

-- testing toArrayOf with traversed and a couple of prisms
data ABC = A (Array XYZ) | B | C
data XYZ = X Number | Y | Z

_A :: Prism' ABC (Array XYZ)
_A = prism' A case _ of
  (A array) -> Just array
  _ -> Nothing

_X :: Prism' XYZ Number
_X = prism' X case _ of
  (X number) -> Just number
  _ -> Nothing

arrayOfNumbers :: ABC -> Array Number
arrayOfNumbers = toArrayOf (_A <<< traversed <<< _X)

-- check we can compose getters
fooBarGetter :: forall x. Getter' { foo :: { bar :: x } } x
fooBarGetter = foo <<< bar

type Foo a = { foo :: Maybe { bar :: Array a } }

doc :: Foo String
doc = { foo: Just { bar: [ "Hello", " ", "World" ] } }

bars :: forall a b. Traversal (Foo a) (Foo b) a b
bars = foo <<< _Just <<< bar <<< traversed

-- Get the index of a deeply nested record
type BarRec = { foo :: Array (Either { bar :: Array Int } String) }
data Bar = Bar BarRec

newtype BarIndex = BarIndex Int

_Bar :: Lens' Bar BarRec
_Bar = lens (\(Bar rec) -> rec) (\_ -> Bar)

doc2 :: Bar
doc2 = Bar { foo: [ Left { bar: [ 1, 2, 3 ] } ] }

_0Justbar :: Traversal' Bar (Either { bar :: Array Int } String)
_0Justbar = _Bar <<< foo <<< ix 0

_1bars :: Traversal' Bar Int
_1bars = _0Justbar <<< _Left <<< bar <<< ix 1

_2Justbared :: IndexedTraversal' BarIndex Bar (Either { bar :: Array Int } String)
_2Justbared = _Bar <<< foo <<< reindexed BarIndex itraversed

-- Tests state using zoom
stateTest :: Tuple Int String
stateTest = evalState go (Tuple 4 [ "Foo", "Bar" ])
  where
  go = Tuple <$> zoom _1 get <*> zoom (_2 <<< traversed) get

--test cloning of indexed lenses
cloneTest :: Tuple Int (Tuple Int Int)
cloneTest = iover (cloneIndexedLens i_2) Tuple (Tuple 1 2)

i_2 :: forall a b c. IndexedLens Int (Tuple a b) (Tuple a c) b c
i_2 = ilens (\(Tuple _ b) -> Tuple 0 b) (\(Tuple a _) b -> Tuple a b)

-- Grates
aGrateExample :: forall a b. Grate (Tuple a a) (Tuple b b) a b
aGrateExample = grate \f -> Tuple (f fst) (f snd)

collectOfTest :: forall f a b. Functor f => (a -> Tuple b b) -> f a -> Tuple (f b) (f b)
collectOfTest = collectOf aGrateExample

summing :: Tuple Int Int -> Tuple Int Int -> Tuple Int Int
summing = zipWithOf (cloneGrate aGrateExample) (+)

-- Test cloning of traversals
cloneTraversalTest :: Maybe Int
cloneTraversalTest = do
  let
    ix1 :: Traversal' (Array Int) Int
    ix1 = ix 1

    wrapper :: { traversal :: ATraversal' (Array Int) Int }
    wrapper = { traversal: ix1 }

  preview (cloneTraversal wrapper.traversal) [ 0, 1, 2 ]

-- lensStore example
data LensStoreExample = LensStoreA Int | LensStoreB (Tuple Boolean Int)

lensStoreExampleInt :: Lens' LensStoreExample Int
lensStoreExampleInt = lens' case _ of
  LensStoreA i -> map LensStoreA <$> lensStore identity i
  LensStoreB i -> map LensStoreB <$> lensStore _2 i

main :: Effect Unit
main = do
  assertEqual' """view bars doc"""
    { expected: "Hello World"
    , actual: view bars doc
    }
  assertEqual' """view barAndFoo { bar: "bar", foo: "foo" }"""
    { expected: Tuple "bar" "foo"
    , actual: view barAndFoo { bar: "bar", foo: "foo" }
    }
  assertEqual' """doc2 ^? _1bars"""
    { expected: Just 2
    , actual: doc2 ^? _1bars
    }
  assertEqual' """arrayOfNumbers $ A [(X 1.0), (X 2.0), (X 3.0)]"""
    { expected: [ 1.0, 2.0, 3.0 ]
    , actual: arrayOfNumbers $ A [ (X 1.0), (X 2.0), (X 3.0) ]
    }
  assertEqual' """unsafePartial $ doc2 ^?! _1bars"""
    { expected: 2
    , actual: unsafePartial $ doc2 ^?! _1bars
    }
  assertEqual' """unsafePartial $ Tuple 0 1 ^@?! i_2"""
    { expected: Tuple 0 1
    , actual: unsafePartial $ Tuple 0 1 ^@?! i_2
    }
  assertEqual' """stateTest"""
    { expected: Tuple 4 "FooBar"
    , actual: stateTest
    }
  assertEqual' """cloneTest"""
    { expected: Tuple 1 (Tuple 0 2)
    , actual: cloneTest
    }
  assertEqual' """summing (Tuple 1 2) (Tuple 3 4)"""
    { expected: Tuple 4 6
    , actual: summing (Tuple 1 2) (Tuple 3 4)
    }
  assertEqual' """collectOfTest (\a -> Tuple (a + a) (a * a)) [4, 5]"""
    { expected: Tuple [ 8, 10 ] [ 16, 25 ]
    , actual: collectOfTest (\a -> Tuple (a + a) (a * a)) [ 4, 5 ]
    }
  assertEqual' """cloneTraversalTest"""
    { expected: Just 1
    , actual: cloneTraversalTest
    }
