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
type bracket = Bracket.t
type srcloc = {ln: int, ch: int}
type srcrange = {begin: srcloc, end: srcloc}
type annotated<'t> = {it: 't, ann: srcrange}
type sequenceKind = List | Vector
type rec t = Atom(atom) | Sequence(sequenceKind, bracket, list<annotated<t>>)
let toString: annotated<t> => string
module Error: {
  type t =
    | WantListFoundEOF
    | WantStringFoundEOF
    | WantOpenBracketFound(string)
    | WantEscapableCharFound(string)
    | MismatchedBracket(bracket, bracket)
  let toString: t => string
}
let fromStringBeginning: string => (annotated<t>, int)
let fromString: string => Belt.List.t<annotated<t>>
