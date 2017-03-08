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
  - Requires file resolving
- Jump to definition
  - Requires definition parsing
- Jump to documentation (optionally inline like a vim help file)
- Find usages
  - Requires project crawling (use tool like ag or pt for finding candidates?)
- Fix case statements after expanding a union type
  - Requires some type inference (if I'm casing on `foo.bar`, I need to be able to know the type of `bar`)
- Add documentation comments to current module
- Autocompletion (based on types)
  - Requires type inference
- Show type of variable
  - Requires type inference
- Sort imports
- (Un)expose a declaration from a module

## Techniques

### File Resolving
- Already did this once in Vimscript

### Definition parsing
Multiple options:
- Parse elm interface files
  - Does this work for local (`let, in`) or unexported definitions?
  - Requires a recent compilation for best results.
  - Probably faster though.
- Selectively parse all definitions out of a file
  - `foo = ...`, `type Foo = A | B`, `type alias Foo = ...`
  - ignore other constructs for speed, and to succeed even if the surrounding code is invalid.
  - Fully qualify definitions: `foo` to `My.Module.foo`
  - Parse what's imported and exposed
  - Determine the scope of a definition (exported, top level or `let, in`)
  - If easy, extract the full code of the definition unparsed for later analysis.

### Crawl project for usages
- Perhaps use a tool like ag or pt to create an initial pool of candidates?
  - Given a fully quaifified definition `My.Module.foo`, any candidate file will always contain both the namespace `My.Module` and the unqualified definition `foo`.
- Check if each candidate file actually exposes the definition.
- Do not forget module including definition itself.

## Next steps
- Let's start with looking at definitions
