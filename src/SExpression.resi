type atom = Str(string) | Sym(string)
type bracket = Round | Square
type sourcePoint = {ln: int, ch: int}
type sourceLocation = {begin: sourcePoint, end: sourcePoint}
type sequenceKind = List | Vector
type error =
  | WantListFoundEOF
  | WantStringFoundEOF
  | WantEscapableCharFound(string)
  | MismatchedBracket(bracket, bracket)
  | ExtraClosingBracket(bracket, sourcePoint)
type annotated<'it, 'ann> = {it: 'it, ann: 'ann}

type rec sexprNode =
  | Atom(atom)
  | Sequence({sequenceKind: sequenceKind, bracket: bracket, content: list<sexpr>})
and sexpr = annotated<sexprNode, sourceLocation>

module Atom: {
  type t = atom
  let toString: atom => string
}
module Bracket: {
  type t = bracket
  let toString: bracket => string
  let toWrapper: bracket => (string, string)
}
module SourcePoint: {
  type t = sourcePoint
  let toString: sourcePoint => string
}
module SourceLocation: {
  type t = sourceLocation
  let toString: sourceLocation => string
}
module SequenceKind: {
  type t = sequenceKind
  let toString: sequenceKind => string
}
module Error: {
  type t = error
  let toString: t => string
}
type exn +=  SExpressionError(error)

module SExpr: {
  type t = sexpr
  let toString: sexpr => string
  type source = {srcloc: SourcePoint.t, i: int, content: string}
  let fromStringBeginning: (~ignoreLangLine:bool=?, string) => option<(t, int)>
  let fromString: (~ignoreLangLine:bool=?, string) => RescriptCore.List.t<t>
}
