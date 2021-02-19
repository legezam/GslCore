module GslCore.Ast.Types.Lex

open FSharp.Text.Lexing


/// Convert matched characters during lexing into a string.
let lexeme = LexBuffer<_>.LexemeString