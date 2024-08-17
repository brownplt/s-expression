open Belt

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

module Atom = {
  type t = atom

  let toString = x => {
    let escape = s => {
      let s = Js.String.replaceByRe(%re("/\\n/g"), "\n", s)
      let s = Js.String.replaceByRe(%re("/\\t/g"), "\t", s)
      let s = Js.String.replaceByRe(%re("/\\r/g"), "\r", s)
      let s = Js.String.replaceByRe(%re("/\\\\/g"), "\\", s)
      let s = Js.String.replaceByRe(%re("/\\\"/g"), "\"", s)
      s
    }
    switch x {
    | Sym(s) => s
    | Str(s) => `"${escape(s)}"`
    }
  }
}

module Bracket = {
  type t = bracket
  let toString = t => {
    switch t {
    | Square => "square"
    | Round => "round"
    }
  }
  let toWrapper = t => {
    switch t {
    | Square => ("[", "]")
    | Round => ("(", ")")
    }
  }
}

module SourcePoint = {
  type t = sourcePoint
  let toString = ({ln, ch}) => `${ln + 1 |> Int.toString}:${ch + 1 |> Int.toString}`
}

module SourceLocation = {
  type t = sourceLocation
  let toString = ({begin, end}) => {
    `${begin |> SourcePoint.toString}-${end |> SourcePoint.toString}`
  }
}

module SequenceKind = {
  type t = sequenceKind
  let toString = t => {
    switch t {
    | List => "list"
    | Vector => "vector"
    }
  }
}

module Error = {
  type t = error
  let toString: t => string = err => {
    switch err {
    | WantListFoundEOF => "reached the end of the file while processing a list."
    | WantStringFoundEOF => "reached the end of the file while processing a string."
    | WantEscapableCharFound(string) => `found an unexpected escape sequence (\\${string}).`
    | MismatchedBracket(start, end) =>
      `found a closing ${Bracket.toString(
          end,
        )} bracket while processing a list started with a ${Bracket.toString(start)} bracket.`
    | ExtraClosingBracket(bracket, srcpt) =>
      `found an extra closing ${Bracket.toString(bracket)} bracket at ${SourcePoint.toString(
          srcpt,
        )}.`
    }
  }
}
exception SExpressionError(error)

module SExpr = {
  type t = sexpr
  let rec toString = (e: sexpr): string =>
    switch e.it {
    | Atom(x) => Atom.toString(x)
    | Sequence(Vector, b, xs) => {
        let (a, z) = Bracket.toWrapper(b)
        `#${a}${String.concat(" ", xs->List.map(toString))}${z}`
      }
    | Sequence(List, b, xs) => {
        let (a, z) = Bracket.toWrapper(b)
        `${a}${String.concat(" ", xs->List.map(toString))}${z}`
      }
    }

  type source = {srcloc: SourcePoint.t, i: int, content: string}

  let annotate = (it, begin, end) => {
    {it, ann: ({begin, end}: sourceLocation)}
  }

  let stringAsSource = s => {
    {srcloc: {ln: 0, ch: 0}, i: 0, content: s}
  }

  let advance = (srcloc: SourcePoint.t, char) => {
    let {ln, ch} = srcloc
    if char === "\n" {
      {ln: ln + 1, ch: 0}
    } else {
      {ln, ch: ch + 1}
    }
  }

  let caseSource = (source): option<(string, source)> => {
    let {srcloc, i, content} = source
    if i < Js.String.length(content) {
      let ch = Js.String.get(content, i)
      let srcloc = advance(srcloc, ch)
      Some((ch, {srcloc, i: i + 1, content}))
    } else {
      None
    }
  }

  let raiseError = x => raise(SExpressionError(x))

  let parseSymbol = (start, firstCh, src: source): (sexpr, source) => {
    let rec loop = (cs, src: source): (sexpr, source) => {
      let end = () => {
        let e = Atom(Sym(String.concat("", List.reverse(cs))))
        (annotate(e, start, src.srcloc), src)
      }
      switch caseSource(src) {
      | None => end()
      | Some(("(", _src)) => end()
      | Some((")", _src)) => end()
      | Some(("[", _src)) => end()
      | Some(("]", _src)) => end()
      | Some((`"`, _src)) => end()
      | Some((chr, src1)) =>
        if Js.Re.test_(%re("/\s+/ig"), chr) {
          end()
        } else {
          let src = src1
          loop(list{chr, ...cs}, src)
        }
      }
    }
    loop(list{firstCh}, src)
  }

  let parseString = (start: sourcePoint, src: source): (t, source) => {
    let rec loop = (cs, src): (t, source) => {
      switch caseSource(src) {
      | None => raiseError(WantStringFoundEOF)
      | Some((`"`, src)) => {
          let e = Atom(Str(String.concat("", List.reverse(cs))))
          (annotate(e, start, src.srcloc), src)
        }

      | Some((chr, src)) =>
        if chr == "\\" {
          escaping(cs, src)
        } else {
          loop(list{chr, ...cs}, src)
        }
      }
    }
    and escaping = (cs, src): (t, source) => {
      switch caseSource(src) {
      | None => raiseError(WantStringFoundEOF)
      | Some((chr, src)) =>
        switch chr {
        | `"` => loop(list{`"`, ...cs}, src)
        | "r" => loop(list{"\r", ...cs}, src)
        | "t" => loop(list{"\t", ...cs}, src)
        | "n" => loop(list{"\n", ...cs}, src)
        | chr =>
          if chr == "\\" {
            loop(list{"\\", ...cs}, src)
          } else {
            raiseError(WantEscapableCharFound(chr))
          }
        }
      }
    }
    loop(list{}, src)
  }

  // internal exceptions
  exception EOF

  let rec forwardToEOL = (src, then) => {
    switch caseSource(src) {
    | None => raise(EOF)
    | Some(("\n", src)) => then(src)
    | Some((_, src)) => forwardToEOL(src, then)
    }
  }

  exception FoundRP(bracket, source)
  let rec parseOne = (src: source): (t, source) => {
    let start = src.srcloc
    switch caseSource(src) {
    | None => raise(EOF)
    | Some(("'", src)) => {
        let (e, src) = parseOne(src)
        (
          annotate(
            Sequence(List, Round, list{annotate(Atom(Sym("quote")), start, src.srcloc), e}),
            start,
            src.srcloc,
          ),
          src,
        )
      }
    | Some(("#", src)) =>
      switch caseSource(src) {
      | None => raise(EOF)
      | Some(("(", src)) => startParseList(Vector, Round, start, src)
      | Some(("[", src)) => startParseList(Vector, Square, start, src)
      | Some((";", src)) => {
          let (_, src) = parseOne(src)
          parseOne(src)
        }
      | Some((_chr, _src)) => parseSymbol(start, "#", src)
      }
    | Some(("(", src)) => startParseList(List, Round, start, src)
    | Some(("[", src)) => startParseList(List, Square, start, src)
    | Some((")", src)) => raise(FoundRP(Round, src))
    | Some(("]", src)) => raise(FoundRP(Square, src))
    | Some((`"`, src)) => parseString(start, src)
    | Some((`;`, src)) => forwardToEOL(src, parseOne)
    | Some((chr, src)) =>
      // Js.log(`This one character is: "${chr}".`)
      if Js.Re.test_(%re("/\s+/ig"), chr) {
        parseOne(src)
      } else {
        parseSymbol(start, chr, src)
      }
    }
  }
  and startParseList = (sequenceKind, bracket1, start, src): (
    t,
    source,
  ) => {
    let rec parseList = (elms, src): (t, source) => {
      switch parseOne(src) {
      | (elm, src) => parseList(list{elm, ...elms}, src)
      | exception EOF => raiseError(WantListFoundEOF)
      | exception FoundRP(bracket2, src) =>
        if bracket1 == bracket2 {
          let e = Sequence(sequenceKind, bracket1, List.reverse(elms))
          (annotate(e, start, src.srcloc), src)
        } else {
          raiseError(MismatchedBracket(bracket1, bracket2))
        }
      }
    }
    parseList(list{}, src)
  }

  let fromStringBeginning = (src: string) => {
    switch parseOne(stringAsSource(src)) {
    | (term, src) => Some(term, src.i)
    | exception EOF => None
    | exception FoundRP(bracket, src) => raiseError(ExtraClosingBracket(bracket, src.srcloc))
    }
  }

  let fromString = (src: string) => {
    let rec loop = (elms, src) => {
      switch parseOne(src) {
      | (elm, src) => loop(list{elm, ...elms}, src)
      | exception EOF => List.reverse(elms)
      | exception FoundRP(bracket, src) => raiseError(ExtraClosingBracket(bracket, src.srcloc))
      }
    }
    loop(list{}, stringAsSource(src))
  }
}
