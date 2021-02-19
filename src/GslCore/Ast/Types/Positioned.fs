namespace GslCore.Ast.Types

open FSharp.Text.Lexing


// TODO: We may want to collapse the distinction between Positioned and Node.  At present they
// are identical types except for the record field names.  We may wish to leave them distinct to
// avoid adding cruft to the lexer type, leaving Node free to accumulate more parsing-related
// fields that we don't want to have to add placeholders to.

/// Generic lexer token that stores lexing position.
type Positioned<'T> = { Item: 'T; Position: SourcePosition }

type PUnit = Positioned<unit>
type PString = Positioned<string>
type PInt = Positioned<int>
type PFloat = Positioned<float>

module Positioned =
    /// Tokenize the item in the lex buffer using a pased conversion function.
    let tokenize (operator: string -> 'a) (lexbuf: LexBuffer<char>): Positioned<'a> =
        let item = operator (Lex.lexeme lexbuf)

        { Positioned.Item = item
          Position = SourcePosition.fromLexbuf lexbuf }

    /// Create a unit token with position from lexbuf.
    let tokenizeUnit: LexBuffer<char> -> PUnit = tokenize (ignore)

    /// Tokenize a lex item as a string.
    let tokenizeString: LexBuffer<char> -> PString = tokenize id

    /// Tokenize a lex item as a string literal, stripping off the quotes.
    let tokenizeStringLiteral: LexBuffer<char> -> PString =
        tokenize (fun stringWithQuotes -> stringWithQuotes.[1..stringWithQuotes.Length - 2])

    /// Tokenize a lex item as an int.
    let tokenizeInt: LexBuffer<char> -> PInt = tokenize int

    /// Tokenize a lex item as a float.
    let tokenizeFloat: LexBuffer<char> -> PFloat = tokenize float

    /// Tokenize a pragma name by trimming the first character
    let tokenizeStringTrimFirstChar (lexbuf: LexBuffer<char>): PString =
        let name = (Lex.lexeme lexbuf).[1..]

        { Positioned.Item = name
          Position = SourcePosition.fromLexbuf lexbuf }

    /// Create a new position bracketed by a pair of positions.
    let posBracketTokens (left: Positioned<'a>) (right: Positioned<'b>): SourcePosition =
        { SourcePosition.Start = left.Position.Start
          End = right.Position.End }


