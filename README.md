# S-expression

An S-expression parser written in ReScript.

## Supported Features

- strings
- symbols
- lists in `( )`
- lists in `[ ]`

## Not supported feature

- all sorts of comments:
  - line comments `; `
  - block comments `#| |#`
  - S-expression comments `#; `
- quoted expressions (`'<something>` means `(quote <something>)` in Lisp)
