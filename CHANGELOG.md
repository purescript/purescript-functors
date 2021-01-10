# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v3.1.1](https://github.com/purescript/purescript-functors/releases/tag/v3.1.1) - 2018-11-30

Reordered instance chain for `Inject` so that `inj :: f ~> f` succeeds

## [v3.1.0](https://github.com/purescript/purescript-functors/releases/tag/v3.1.0) - 2018-10-13

Added `FunctorWithIndex`, `FoldableWithIndex`, `TraversableWithIndex` instances (@MonoidMusician)

## [v3.0.1](https://github.com/purescript/purescript-functors/releases/tag/v3.0.1) - 2018-06-28

- Fixed broken `Eq` and `Eq1` instances for `Compose`

## [v3.0.0](https://github.com/purescript/purescript-functors/releases/tag/v3.0.0) - 2018-05-23

- Updated for PureScript 0.12
- Added `Inject` class for injecting values into/projecting values out of nested `Coproduct`s

## [v2.2.0](https://github.com/purescript/purescript-functors/releases/tag/v2.2.0) - 2017-07-07

Add `Monoid App` instance.

## [v2.1.0](https://github.com/purescript/purescript-functors/releases/tag/v2.1.0) - 2017-05-27

Add `MonadZero App` instance.

## [v2.0.0](https://github.com/purescript/purescript-functors/releases/tag/v2.0.0) - 2017-03-26

- Updated for PureScript 0.11
- Renamed the `/\` nexted product operator to `</\>`, since `/\` is also the nested tuple operator
- Added corresponding type operator for nested `Product`
- Added `<\/>` operators for nested `Coproduct`

## [v1.1.0](https://github.com/purescript/purescript-functors/releases/tag/v1.1.0) - 2017-01-29

- Added `Extend` and `Comonad` instances for `Coproduct` (@syaiful6)

## [v1.0.0](https://github.com/purescript/purescript-functors/releases/tag/v1.0.0) - 2016-10-11

New edition for the PureScript 0.10 compiler.

## [v0.1.2](https://github.com/purescript/purescript-functors/releases/tag/v0.1.2) - 2016-05-02

Fixes for pursuit.

## [v0.1.1](https://github.com/purescript/purescript-functors/releases/tag/v0.1.1) - 2016-04-30

- More instances (@zrho)

## [v0.1.0](https://github.com/purescript/purescript-functors/releases/tag/v0.1.0) - 2015-08-19

Initial versioned release

