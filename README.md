# Elm tools
As a Vim and Elm enthousiast, I'd like to improve the quality of the tools available for writing Elm in Vim.
This repo is the home of my experiments.

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
- Fix case statements after expanding a union type
- Add documentation comments to current module
- Autocompletion (based on types)
- Show type of variable
- Sort imports
- (Un)expose a declaration from a module

## Steps
- Parser to generate elm tag files, learn about parsers in the process.
