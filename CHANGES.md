# Version History

## 1.1 (October 27, 2020)

  - New `Shell` module for building shell-like interfaces.

  - New completion helper function `completionFromList`.

  - Update dependencies to their latest versions

  - Minor updates:

    - 1.1.0.1 (October 27, 2020): Limit `optparse-applicative` to 0.16.x
    - 1.1.1 (June 2, 2021): Build with GHC 9.0.1
    - 1.1.2 (January 20, 2023): Build with GHC 9.2.4 (thanks @Arraying)

## 1.0 (July 27, 2020)

This version is the result of a major refactoring of the code in order
to produce an MTL-compatible library.

  - New MTL-style class: `MonadByline`

  - Proper monad transformer: `BylineT`

  - Most of the library is under a single import: `Byline`

  - Added a `ToStylizedText` class to enable using custom types with
    functions like `menu`.

  - New `Byline.Exit.die` function to exit the current process with a
    stylized error message.  Thanks to the `ToStylizedText` class it's
    easy to exit with custom error types.

  - The `Report` type and associated functions were superfluous and
    therefore removed.  (Consider using the new `die` and `warn`
    functions in `Byline.Exit`.)

  - Menus now use `NonEmpty` to represent items and therefore the menu
    `Choice` type has been simplified, removing the `NoItems`
    constructor.

  - Fixed a bug where `Stylized Text` was not rendered when using one
    of the `ask*` functions (#1).

  - Proper encoding of escape sequences so Haskeline doesn't
    [print garbage](https://github.com/judah/haskeline/issues/130) on Windows.

  - Added support for RGB terminals

  - Added an implementation of 'MonadByline' that uses simulated user
    input to test your Byline code.

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
