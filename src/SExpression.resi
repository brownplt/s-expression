module Atom: {
  type t = Str(string) | Sym(string)
  let toString: t => string
}
type atom = Atom.t
module Bracket: {
  type t = Round | Square
  let toString: t => string
  let toWrapper: t => (string, string)
}
module SrcLoc: {
  type t = {ln: int, ch: int}
  let toString: t => string
}
type srcrange = {begin: SrcLoc.t, end: SrcLoc.t}
type annotated<'t> = {it: 't, ann: srcrange}
type sequenceKind = List | Vector
type rec t = Atom(atom) | Sequence(sequenceKind, Bracket.t, list<annotated<t>>)
let toString: annotated<t> => string
module Error: {
  type t =
    | WantListFoundEOF
    | WantStringFoundEOF
    | WantEscapableCharFound(string)
    | MismatchedBracket(Bracket.t, Bracket.t)
    | ExtraClosingBracket(Bracket.t, SrcLoc.t)
  let toString: t => string
}
type exn += SExpressionError(Error.t)
type exn += FoundNoSExpression
let fromStringBeginning: string => (annotated<t>, int)
let fromString: string => Belt.List.t<annotated<t>>
