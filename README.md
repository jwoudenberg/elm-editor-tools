# Elm tools
Tools for querying and manipulating elm files.
Focussed on vim support.

## Similar
- [ElmJutsu](https://atom.io/packages/elmjutsu)
  - Uses regexes.
- [Elm Oracle](https://github.com/ElmCast/elm-oracle)
  - Written in Elm
- [Emacs Elm-Mode](https://github.com/jcollard/elm-mode)
  - Generates tags
  - Elm-Oracle is used for auto-completion
- [Elm Light](https://github.com/rundis/elm-light)
  - Uses its own AST
- [Elm Reflection](https://github.com/stoeffel/elm-reflection)

## Possible backends
- Vim Ctags
- Parser and AST

## Possible languages
- Node
- Haskell
- Elm
- Vimscript

## Features
These are ordered in decreasing priority for me personally.
- Jump to file
- Jump to definition
- Jump to documentation (optionally inline like a vim help file)
- Find usages
- Add documentation comments to current module
- Autocompletion (based on types)
- Show type of variable
- Sort imports
- (Un)expose a declaration from a module

## Steps
- Parser to generate elm tag files, learn about parsers in the process.
