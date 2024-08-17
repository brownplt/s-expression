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
type rec sexpr = annotated<sexprNode, sourceLocation>
and sexprNode = Atom(atom) | Sequence(sequenceKind, bracket, list<sexpr>)

module Atom: {
  type t = atom
  let toString: t => string
}

module Bracket: {
  type t = bracket
  let toString: t => string
  let toWrapper: t => (string, string)
}

// a point in the source file
module SourcePoint: {
  type t = sourcePoint
  let toString: t => string
}

// a location in the source file
module SourceLocation: {
  type t = sourceLocation
  let toString: t => string
}

module SequenceKind: {
  type t = sequenceKind
  let toString: t => string
}

module Error: {
  type t = error
  let toString: t => string
}
type exn += SExpressionError(error)

module SExpr: {
  type rec t = sexpr
  let toString: t => string
  let fromStringBeginning: string => option<(t, int)>
  let fromString: string => list<t>
}
