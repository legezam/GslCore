// Signature file for parser generated by fsyacc
module GslParser
type token = 
  | LPAREN
  | RPAREN
  | PLUS
  | NEWLINE
  | EOF
  | UMINUS
  | DOUBLEQUOTE
  | START_ROUGHAGE
  | END_ROUGHAGE
  | GREATERTHAN
  | LESSTHAN
  | DOT
  | LET
  | CUT
  | END
  | OPEN
  | FOR
  | IN
  | DO
  | COLON
  | STAR
  | SLASH
  | AT
  | LBRACE
  | RBRACE
  | EXCLM
  | EQUALS
  | CARAT
  | COMMA
  | HYPHEN
  | OPENSQBRACKET
  | DOLLAR
  | CLOSESQBRACKET
  | SEMICOLON of (PUnit)
  | MARKER of (PUnit)
  | TILDE of (PUnit)
  | VARIABLE of (PString)
  | PNAME of (PString)
  | PVALUE of (PString)
  | DNAMUTATION of (PString)
  | AAMUTATION of (PString)
  | LINKER of (PString)
  | QUOTED_STRING of (PString)
  | DOCSTRING of (PString)
  | STRING of (PString)
  | INT of (PInt)
  | ID of (PString)
type tokenId = 
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_PLUS
    | TOKEN_NEWLINE
    | TOKEN_EOF
    | TOKEN_UMINUS
    | TOKEN_DOUBLEQUOTE
    | TOKEN_START_ROUGHAGE
    | TOKEN_END_ROUGHAGE
    | TOKEN_GREATERTHAN
    | TOKEN_LESSTHAN
    | TOKEN_DOT
    | TOKEN_LET
    | TOKEN_CUT
    | TOKEN_END
    | TOKEN_OPEN
    | TOKEN_FOR
    | TOKEN_IN
    | TOKEN_DO
    | TOKEN_COLON
    | TOKEN_STAR
    | TOKEN_SLASH
    | TOKEN_AT
    | TOKEN_LBRACE
    | TOKEN_RBRACE
    | TOKEN_EXCLM
    | TOKEN_EQUALS
    | TOKEN_CARAT
    | TOKEN_COMMA
    | TOKEN_HYPHEN
    | TOKEN_OPENSQBRACKET
    | TOKEN_DOLLAR
    | TOKEN_CLOSESQBRACKET
    | TOKEN_SEMICOLON
    | TOKEN_MARKER
    | TOKEN_TILDE
    | TOKEN_VARIABLE
    | TOKEN_PNAME
    | TOKEN_PVALUE
    | TOKEN_DNAMUTATION
    | TOKEN_AAMUTATION
    | TOKEN_LINKER
    | TOKEN_QUOTED_STRING
    | TOKEN_DOCSTRING
    | TOKEN_STRING
    | TOKEN_INT
    | TOKEN_ID
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_Final
    | NONTERM_ScopedBlock
    | NONTERM_CodeSection
    | NONTERM_Line
    | NONTERM_PragmaValue
    | NONTERM_PragmaValues
    | NONTERM_Pragma
    | NONTERM_Pragmas
    | NONTERM_InlinePragmas
    | NONTERM_TypedVariableDeclaration
    | NONTERM_FunctionDefArgs
    | NONTERM_FunctionDeclaration
    | NONTERM_TypedValue
    | NONTERM_CommaSeparatedTypedValues
    | NONTERM_FunctionCall
    | NONTERM_IntLiteral
    | NONTERM_FloatLiteral
    | NONTERM_StringLiteral
    | NONTERM_IntExp
    | NONTERM_StringExp
    | NONTERM_Linker
    | NONTERM_Part
    | NONTERM_PartMaybeMods
    | NONTERM_PartMaybePragma
    | NONTERM_PartFwdRev
    | NONTERM_CompletePart
    | NONTERM_RelPos
    | NONTERM_Slice
    | NONTERM_Mod
    | NONTERM_ModList
    | NONTERM_PartList
    | NONTERM_AssemblyPart
    | NONTERM_L2IdWrap
    | NONTERM_L2Id
    | NONTERM_L2Promoter
    | NONTERM_L2Locus
    | NONTERM_L2ExpElement
    | NONTERM_L2ExpElementList
    | NONTERM_L2ExpLine
    | NONTERM_RID
    | NONTERM_RoughageMarker
    | NONTERM_RoughageMarkerMaybe
    | NONTERM_RoughagePartFwd
    | NONTERM_RoughagePartRev
    | NONTERM_RoughageElement
    | NONTERM_RoughageElementList
    | NONTERM_RoughageLocus
    | NONTERM_RoughageLine
    | NONTERM_RoughageLineList
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (AstTreeHead) 
