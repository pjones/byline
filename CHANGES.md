# Version History

## 1.0.0.0 (???)

This version is the result of a major refactoring of the code to
produce an MTL compatible library.

  - New MTL-style class: `MonadByline`

  - Proper monad transformer: `BylineT`

  - Everything is under a single import: `Byline`

  - [ ] MTL instances for `MonadByline` and `BylineT`

  - [ ] Fix completion bug caused by Haskeline not knowing where the
        prompt is since we write the prompt directly to the output
        handle.

## 0.4.0.0 (March 17, 2020)

  - The `askUntil` function is now polymorphic in its return type.
    This allows the confirmation function to change the return type.

  - Removed `Data.Monoid` backwards compatibility for older GHCs.

  - Update dependency bounds

## 0.3.2.1 (April 15, 2019)

  - Update dependency bounds

  - Builds on NixOS 18.09, 19.03, and unstable

## 0.3.2.0 (October 09, 2018)

  - Update dependency bounds

## 0.3.1.0 (March 20, 2018)

  - Update dependency bounds

  - Deal with the Semigroup Monoid Proposal

## 0.3.0.0 (December 1, 2017)

  - New style modifier: `swapFgBg` (Thanks to Sam Tay)

  - `Byline` is now an instance of `MonadTrans` (Thanks to Sam Tay)

## 0.2.4.0 (March 19, 2017)

  - Update dependency bounds

  - Add Travis CI build status for supported versions of GHC

## 0.2.3.0 (November 1, 2016)

  - Increase upper bound on transformers package to `0.5` (Thanks to
    slycelote).

## 0.2.2.0 (June 9, 2016)

  - Build under LTS-5.15

## 0.2.1.0 (November 5, 2014)

  - Updated dependencies

## 0.2.0.0 (May 22, 2015)

  - Added the NoItems constructor for Choice to deal with menus that
    are asked to display an empty list.

  - Changes to build with GHC 7.8.4. and 7.10.1

## 0.1.0.0 (May 19, 2015)

  - Initial release.
