namespace GslCore.Ast.Types

open FSharp.Text.Lexing
open GslCore
open GslCore.Constants

type SourcePosition =
    { Start: Position
      End: Position }
    override this.ToString() =
        sprintf "@%d,%d-%d,%d" (this.Start.Line + 1) (this.Start.Column + 1) (this.End.Line + 1) (this.End.Column + 1)

module SourcePosition =
    /// Return a nicely-formatted message for the start of this source position.
    let format (this: SourcePosition): string =
        sprintf "near line %d col %d" (this.Start.Line + 1) (this.Start.Column + 1)
    /// Provide a code snippet with an indication of the start of this position.
    /// Returned as a sequence of strings, one sequence item for each line.
    /// Optionally override the default number of lines to use for context, defaults to 5.
    let sourceContextWithSize (GslSourceCode source) (contextLines: int) (this: SourcePosition): string seq =
        seq {

            let lines =
                source
                    .Replace("\r\n", "\n")
                    .Split([| '\n'; '\r' |])

            let p = this.Start

            for line in max 0 (p.Line - contextLines) .. min (p.Line + contextLines) (lines.Length - 1) do
                yield sprintf "%s" lines.[line]
                if line = p.Line then yield sprintf "%s^" (Utils.pad p.Column)
        }

    let sourceContext source this = sourceContextWithSize source 5 this

    /// Expand possibly multiple levels of source positions into a formatted string
    let formatSourcePositionList (positions: SourcePosition list) =
        positions |> List.map format |> String.concat "; "

    let empty =
        { SourcePosition.Start = Position.FirstLine("")
          End = Position.FirstLine("") }

    let fromLexbuf (lexbuf: LexBuffer<_>) =
        { SourcePosition.Start = lexbuf.StartPos
          End = lexbuf.EndPos }

/// Interface type to allow generic retrieval of a source code position.
type ISourcePosition =
    abstract OptionalSourcePosition: SourcePosition list

