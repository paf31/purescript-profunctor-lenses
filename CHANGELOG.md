# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:
- Add `imapped` (#146 by @twhitehead)

Bugfixes:

Other improvements:

## [v8.0.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v8.0.0) - 2022-04-27

Breaking changes:
- Update project and deps to PureScript v0.15.0 (#141 by @JordanMartinez)
- Replaced polymorphic proxies with monomorphic `Proxy` (#141 by @JordanMartinez)

New features:
- Add `coerced` (#140 by @ozkutuk)
- Add `sans` and `both` (#97 by @xgrommx)

Bugfixes:
- Fix broken `reindex` for v0.15 due to [Purescript PR #4033](https://github.com/purescript/purescript/pull/4033)

Other improvements:
- Added `purs-tidy` formatter (#138 by @thomashoneyman)
- Replace manual tests with automated tests using `assert` (#135 by @neppord)
- Improve documentation for `united` (#134 by @neppord)
- Add guide on impredicativity explaining difference between `Lens` vs `ALens` (#136 by @i-am-tom and @JordanMartinez)

## [v7.0.1](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v7.0.1) - 2021-05-06

New features:
- Export `ifindOf`, `iforOf`, and `iforOf_` implemented in #66 by @Rufflewind and #21 by @zrho (#131 by @JordanMartinez)

Other improvements:
- Fix warnings revealed by PS v0.14.1 (#131 by @JordanMartinez)

## [v7.0.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v7.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#122, #123)
- Removed `(++~)` and `(++=)` operators, as they are meant to mimic the long-removed `(++)` in `prelude` (#58)

New features:
- Added `unto` (#93)
- Added affine traversals (#112)

Bugfixes:
- Fixed `collectOf` in the `Grate` module (#63)

Other improvements:
- Changed default branch to `main` from `master`
- Updated to comply with Contributors library guidelines by adding new issue and pull request templates, updating documentation, and migrating to Spago for local development and CI (#108, #121)

## [v6.3.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v6.3.0) - 2020-02-10

This release includes a number of small but meaningful improvements to the library:

- Made `indexList` more efficient and added an `Index` instance for `List` (@cscalfani)
- Added a re-export for `ATraversal` and `ATraversal'` (@pbrant)
- Added a new function, `simple`, which can be used to restrict the type of an optic to aid type inference (@LiamGoodacre)
- Added a new `lensStore` function which can be used to construct a lens when a type appears under every constructor in a sum type (@LiamGoodacre)
- Added `toArrayOf` and `toArrayOfOn` which behave like the existing `toListOf` and `toListOfOn` optics, but for arrays (@dwhitney)

## [v6.2.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v6.2.0) - 2019-04-08

- Added `Index` instance for `NonEmptyArray` (@reactormonk)

## [v6.1.1](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v6.1.1) - 2019-03-27

- Removed unnecessary `Applicative` constraints from `Traversal` functions.

## [v6.1.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v6.1.0) - 2019-03-16

- Added `ATraversal` and `cloneTraversal` (@pbrant)

## [v6.0.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v6.0.0) - 2019-03-04

- Updated dependencies for the latest `-foreign-object` and `-record`

## [v5.0.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v5.0.0) - 2018-10-15

- Made Getters compose and define AGetter/cloneGetter (@LiamGoodacre)

## [v4.0.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v4.0.0) - 2018-05-25

- Updates for 0.12

## [v3.8.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.8.0) - 2017-11-07

- Added various instances for `Tagged`

## [v3.7.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.7.0) - 2017-09-13

- Added `takeBoth` (@coot)

## [v3.6.1](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.6.1) - 2017-08-31

- Added explicit `foldable-traversable` dependency

## [v3.6.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.6.0) - 2017-08-13

- Added `Monoid Forget` instance (@joneshf)

## [v3.5.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.5.0) - 2017-08-13

- Added `Closed Tagged` instance (@joneshf)

## [v3.4.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.4.0) - 2017-07-31

- Now uses `purescript-record` (@coot)

## [v3.3.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.3.0) - 2017-07-27

- Added `iforOf`, `iforOf_`, `itraversed` and `reindexed` (@Rufflewind)

## [v3.2.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.2.0) - 2017-04-13

- Added `prop` lenses for record fields.

## [v3.1.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.1.0) - 2017-04-12

- Added `Grate` and corresponding functions.

## [v3.0.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v3.0.0) - 2017-04-02

- Updated for 0.11.1

## [v2.5.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v2.5.0) - 2017-01-16

- Added `unsafeView` and `unsafeIndexedFold` (@boothead)

## [v2.4.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v2.4.0) - 2017-01-13

- Added indexed lenses (@Roxxik)

## [v2.3.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v2.3.0) - 2016-12-20

- Exported `traverseOf_` (@Roxxik)

## [v2.1.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v2.1.0) - 2016-10-31

- Added `asIndex` (@puffnfresh)

## [v2.0.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v2.0.0) - 2016-10-22

- Updated dependencies
- All `P`-named varieties of optics now use `'` intead (`LensP` -> `Lens'`, etc)
- `_Coproduct` and `_Product` isos have been removed as they fall under the new `_Newtype` iso
- Added functional dependencies to `Index` and `At` (@tslawler)

## [v1.0.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v1.0.0) - 2016-06-02

- Updated for 1.0 core libraries and PureScript 0.9

## [v0.5.4](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.5.4) - 2016-05-02

- Updated `bower.json` (@hdgarrood)

## [v0.5.3](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.5.3) - 2016-04-24

- Exported `iover` (@nathanfaubion)

## [v0.5.2](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.5.2) - 2016-04-21

- Added `Wander` instance to `Indexed` (@tslawler)

## [v0.5.1](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.5.1) - 2016-04-16

- Exported `itraverseOf` and `element` (@zrho)

## [v0.5.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.5.0) - 2016-02-27

- Converted functions to `Forget`, including `to` (@zrho)

## [v0.4.2](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.4.2) - 2016-01-28

- Added `IndexedTraversal` and friends (@zrho)

## [v0.4.1](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.4.1) - 2016-01-24

- Fixed behaviour of `Data.Lens.Fold.minimumOf` (@scott-christopher)

## [v0.4.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.4.0) - 2016-01-12

- Added `zoom` and `MonadState` combinators.
- Added `Re` profunctor to turn around isos into isos, lenses into reviews, etc.

By @zrho

## [v0.3.5](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.3.5) - 2015-11-20

- Removed unused imports

## [v0.3.4](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.3.4) - 2015-11-07

- Reexported `first`, `second`, `left` and `right`.

## [v0.3.3](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.3.3) - 2015-11-02

- Removed unused imports

## [v0.3.2](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.3.2) - 2015-10-30

- Added isomorphisms for `Product`/`Tuple` and `Coproduct`/`Either`
- Added lenses and prisms for `Product` and `Coproduct`
- Split up the existing common predefined lenses and prisms so they can be imported individually by type
- Added more re-exports throughout

## [v0.3.1](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.3.1) - 2015-10-29

- Added `At` and `Index` (@jonsterling)

## [v0.3.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.3.0) - 2015-09-23

- Generalize `Wander`
- Added standard lenses, traversals and folds

by @zrho

## [v0.2.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.2.0) - 2015-09-13

- Added functions based on `purescript-lens` and `purescript-optic` (@zrho)

## [v0.1.0](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases/tag/v0.1.0) - 2015-09-06

- Initial release
