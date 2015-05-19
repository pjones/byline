# Byline -- Library for creating command-line interfaces (colors, menus, etc.)

Byline simplifies writing interactive terminal applications by
building upon [ansi-terminal][] and [haskeline][]. This makes it
possible to print messages and prompts that include terminal escape
sequences such as colors that are automatically disabled when standard
input is a file. It also means that Byline works on both
POSIX-compatible systems and on Windows.

The primary features of Byline include printing messages, prompting
for input, and generating custom menus. It was inspired by the
[highline] Ruby library and the [terminal library][] by Craig Roche.

[ansi-terminal]: http://hackage.haskell.org/package/ansi-terminal
[haskeline]: https://hackage.haskell.org/package/haskeline
[highline]: https://github.com/JEG2/highline
[terminal]: https://github.com/cdxr/terminal
