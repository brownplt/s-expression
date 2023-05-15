open Belt

type srcloc = {ln: int, ch: int}
type srcrange = {begin: srcloc, end: srcloc}
type annotated<'t> = {it: 't, ann: srcrange}
let annotate = (it, begin, end) => {
  {it, ann: {begin, end}}
}

/*
Okay, I find this file gets really confusing. I am using exceptions for two purposes:

1. to backtrack
2. to raise real parsing error

Let's leave the first kind of exceptions as their own kind, and group all second kind
exception in one data type.
*/

type atom =
  | Str(string)
  | Sym(string)

type bracket =
  | Round
  | Square
let bracketAsString = bracket => {
  switch bracket {
  | Square => "square"
  | Round => "round"
  }
}

type sequenceKind =
  | List
  | Vector

type rec s_expr =
  | Atom(atom)
  | Sequence(sequenceKind, bracket, list<annotated<s_expr>>)

type source = {srcloc: srcloc, i: int, content: string}

let stringAsSource = s => {
  {srcloc: {ln: 0, ch: 0}, i: 0, content: s}
}

let advance = (srcloc, char) => {
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

type parseError =
  | WantListFoundEOF
  | WantStringFoundEOF
  | WantOpenBracketFound(string)
  | WantEscapableCharFound(string)
  | MismatchedBracket(bracket, bracket)
exception ParseError(parseError)
let parseErrorAsString: parseError => string = err => {
  switch err {
  | WantListFoundEOF => "Reached the end of the file while processing a list."
  | WantStringFoundEOF => "Reached the end of the file while processing a string."
  | WantOpenBracketFound(string) => `Found an unexpected string (${string}) after \`#\`.`
  | WantEscapableCharFound(string) => `Found an unexpected escape sequence (\\${string}).`
  | MismatchedBracket(start, end) =>
    `Found a closing ${bracketAsString(end)} bracket but this list starts with a ${bracketAsString(
        start,
      )} bracket.`
  }
}

let parseSymbol = (start, firstCh, src: source): (annotated<s_expr>, source) => {
  let rec loop = (cs, src: source): (annotated<s_expr>, source) => {
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

let parseString = (start: srcloc, src: source): (annotated<s_expr>, source) => {
  let rec loop = (cs, src): (annotated<s_expr>, source) => {
    switch caseSource(src) {
    | None => raise(ParseError(WantStringFoundEOF))
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
  and escaping = (cs, src): (annotated<s_expr>, source) => {
    switch caseSource(src) {
    | None => raise(ParseError(WantStringFoundEOF))
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
          raise(ParseError(WantEscapableCharFound(chr)))
        }
      }
    }
  }
  loop(list{}, src)
}

exception EOF
exception WantSExprFoundRP(bracket, source)
let rec parseOne = (src: source): (annotated<s_expr>, source) => {
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
    | Some((chr, _src)) => raise(ParseError(WantOpenBracketFound(chr)))
    }
  | Some(("(", src)) => startParseList(List, Round, start, src)
  | Some(("[", src)) => startParseList(List, Square, start, src)
  | Some((")", src)) => raise(WantSExprFoundRP(Round, src))
  | Some(("]", src)) => raise(WantSExprFoundRP(Square, src))
  | Some((`"`, src)) => parseString(start, src)
  | Some((chr, src)) =>
    // Js.log(`This one character is: "${chr}".`)
    if Js.Re.test_(%re("/\s+/ig"), chr) {
      parseOne(src)
    } else {
      parseSymbol(start, chr, src)
    }
  }
}
and startParseList = (sequenceKind, bracket1, start, src): (annotated<s_expr>, source) => {
  let rec parseList = (elms, src): (annotated<s_expr>, source) => {
    switch parseOne(src) {
    | (elm, src) => parseList(list{elm, ...elms}, src)
    | exception EOF => raise(ParseError(WantListFoundEOF))
    | exception WantSExprFoundRP(bracket2, src) =>
      if bracket1 == bracket2 {
        let e = Sequence(sequenceKind, bracket1, List.reverse(elms))
        (annotate(e, start, src.srcloc), src)
      } else {
        raise(ParseError(MismatchedBracket(bracket1, bracket2)))
      }
    }
  }
  parseList(list{}, src)
}

let parseMany = (src: source) => {
  let rec loop = (elms, src) => {
    switch parseOne(src) {
    | (elm, src) => loop(list{elm, ...elms}, src)
    | exception EOF => List.reverse(elms)
    }
  }
  loop(list{}, src)
}

let rec stringOfSExpr = (e: annotated<s_expr>) =>
  switch e.it {
  | Atom(Sym(s)) => s
  | Atom(Str(s)) => "str:" ++ s
  | Sequence(Vector, _b, list{}) => "#()"
  | Sequence(Vector, _b, list{x, ...xs}) => {
      let stringOfXs = String.concat("", List.map(xs, x => " " ++ stringOfSExpr(x)))
      "#(" ++ stringOfSExpr(x) ++ stringOfXs ++ ")"
    }
  | Sequence(List, _b, list{}) => "()"
  | Sequence(List, _b, list{x, ...xs}) => {
      let stringOfXs = String.concat("", List.map(xs, x => " " ++ stringOfSExpr(x)))
      "(" ++ stringOfSExpr(x) ++ stringOfXs ++ ")"
    }
  }
