namespace GslCore.Ast.ErrorHandling

open GslCore.GslResult
open GslCore.Ast.Types
open FsToolkit.ErrorHandling
open FSharp.Text.Parsing
open System.Diagnostics
open GslCore
open GslCore.Constants

// ================
// error handling support
// ================


/// Enumeration of every possible kind of error.  Most of these will just be flags, some might hold
/// extra data for convenience and to allow the outer message type to remain the standard.
[<RequireQualifiedAccess>]
type AstMessageType =
    | Context // a message which should be interpreted as additional context for a previous message
    | Warning
    | DeprecationWarning // we deduplicate these to avoid inundating the user
    | GeneralError // totally generic error, ideally be more specific
    | ParserError // catchall for ParseError ast nodes for the time being.
    | PartError // catchall for errors related to part validation
    | UnresolvedVariable
    | UnresolvedFunction
    | RecursiveFunctionCall
    | TypeError
    | PragmaError // errors related to pragma construction or manipulation
    | InternalError of AstMessageType // errors that imply WE screwed up somewhere, not the user
    | RefGenomeError
    | ValueError // errors related to values being out of range and such
    // errors encountered when bootstrapping AST elements from source inside the compiler
    // the extra wrapped ast node is intended to be the parsed tree that failed to bootstrap correctly
    | BootstrapError of AstNode option
    // errors related to specific expansion phases that still raise exceptions
    | L2ExpansionError
    | MutationError
    | ProteinError
    | HetBlockError

// TODO: should improve message printing for bootstrapping by dumping source representation of the tree

/// Describe a warning or error encountered during Ast manipulations.
type AstMessage =
    { Message: string
      SourcePosition: SourcePosition option
      Node: AstNode
      Type: AstMessageType
      StackTrace: StackTrace option }
    override this.ToString() = this.Summary

    /// Pretty-print a short summary of an AST message.
    member this.Summary: string =
        let messageTypeName = Utils.getUnionCaseName this.Type
        // get the best position we can
        match this.SourcePosition, this.Node.pos with
        | Some position, _
        | None, Some position -> sprintf "%s: %s\n%s" messageTypeName (position |> SourcePosition.format) this.Message
        | _ -> // can't do much without a position now, can we.
            sprintf "%s: %s" messageTypeName this.Message

// =======================
// helper functions for creating warnings and errors
// =======================
module AstMessage =
    /// Delegate position to a passed node.
    let create (stackTrace: StackTrace option) (msgType: AstMessageType) (msg: string) (node: AstNode): AstMessage =
        { AstMessage.Message = msg
          SourcePosition = node.pos
          Node = node
          Type = msgType
          StackTrace = stackTrace }

    /// Create a message with no stack trace, of Warning type.
    let createWarning = create None AstMessageType.Warning

    /// Create a message that collects a stack trace, with unspecified type.
    let createErrorWithStackTrace = create (Some(StackTrace()))

    let optionalContextStr: string option -> string =
        function
        | Some s -> sprintf " in %s" s
        | None -> ""

type AstResult<'a> = GslResult<'a, AstMessage>


module AstResult =
    let errStringMsg (msgType: AstMessageType) (msg: string) (node: AstNode): AstMessage =
        AstMessage.createErrorWithStackTrace msgType msg node

    let errString (msgType: AstMessageType) (msg: string) (node: AstNode): AstResult<'a> =
        errStringMsg msgType msg node |> GslResult.err

    let errStringFMsg msgType msgfmt fmtVal node: AstMessage =
        AstMessage.createErrorWithStackTrace msgType (sprintf msgfmt fmtVal) node

    let errStringF msgType msgfmt fmtVal node: AstResult<'a> =
        errStringFMsg msgType msgfmt fmtVal node
        |> GslResult.err

    ///Create an error representing a type mismatch resulting from a bugged GSL program.
    let variableTypeMismatchMsg (variableName: string) (declaredType: 'a) (expectedType: 'b) (node: AstNode): AstMessage =
        let message =
            sprintf
                "The variable %s has been inferred to have the type %O, but is required to have the type %O in this context."
                variableName
                declaredType
                expectedType

        errStringMsg AstMessageType.TypeError message node

    let variableTypeMismatch (variableName: string) (declaredType: 'a) (expectedType: 'b) (node: AstNode): AstResult<'a> =
        variableTypeMismatchMsg variableName declaredType expectedType node
        |> GslResult.err


    ///<summary>
    ///Create an internal error representing a type mismatch.
    ///This is a common pattern when unpacking AST entities, and implies
    ///a bug in compiler logic rather than an error in parsed source code.
    ///</summary>
    let internalTypeMismatchMsg (maybeContext: string option) (expectedType: string) (actualNode: AstNode): AstMessage =
        let message =
            sprintf
                "Expected a '%s'%s, but got a '%s'"
                expectedType
                (AstMessage.optionalContextStr maybeContext)
                (actualNode.TypeName)

        errStringMsg (AstMessageType.InternalError(AstMessageType.TypeError)) message actualNode

    let internalTypeMismatch (maybeContext: string option) (expectedType: string) (actualNode: AstNode): AstResult<'a> =
        internalTypeMismatchMsg maybeContext expectedType actualNode
        |> GslResult.err

    ///Create an internal error if we encounter a pragma that hasn't been built.
    let unbuiltPragmaError (context: string option) (name: string) (node: AstNode): AstResult<'a> =
        let message =
            sprintf "Found an unbuilt pragma%s: '%s'" (AstMessage.optionalContextStr context) name

        errString (AstMessageType.InternalError(AstMessageType.PragmaError)) message node

    /// Convert an exception into an error message.
    /// Provide an AST node for context.
    let exceptionToError (msgType: AstMessageType) (astNodeContext: AstNode) (exc: System.Exception): AstMessage =
        let msg = exc.Message

        { AstMessage.Message = msg
          SourcePosition = astNodeContext.pos
          Node = astNodeContext
          Type = msgType
          StackTrace = Some(StackTrace(exc)) }

    /// Pretty-print an AST message including context in source code.
    let getLongForm (showStackTrace: bool, sourceCode: GslSourceCode) (this: AstMessage): string =

        let msgTypeName = Utils.getUnionCaseName this.Type
        // get the best position we can
        let pos =
            match this.SourcePosition, this.Node.pos with
            | Some (p), _
            | None, Some (p) -> Some(p)
            | _ -> None

        match pos with
        | None -> // can't do much without a position now, can we.
            sprintf "%s: %s" msgTypeName this.Message
        | Some (p) -> // now we're cooking with gas
            // Accumulate lines in an error report.
            seq {
                yield (sprintf "%s: %s\n%s" msgTypeName (p |> SourcePosition.format) this.Message)
                yield "================================================================="

                yield! p |> SourcePosition.sourceContext sourceCode

                if showStackTrace then
                    yield this.StackTrace.ToString()
            }
            |> String.concat "\n"

type GslParseErrorContext =
    { stateStack: int list
      parseState: IParseState
      reduceTokens: int list
      currentToken: obj option
      reducibleProductions: int list list
      shiftableTokens: int list
      message: string }

exception GslParseError of GslParseErrorContext

module GslParseErrorContext =

    /// Customized handler for errors that occur during parsing.
    /// Mostly here to eliminate the polymorphism on token type to
    /// allow us to pass the parse error context up stack.
    let handleParseError (context: ParseErrorContext<'tok>): 'a =
        let newContext =
            { stateStack = context.StateStack
              parseState = context.ParseState
              reduceTokens = context.ReduceTokens
              currentToken = context.CurrentToken |> Option.map box
              reducibleProductions = context.ReducibleProductions
              shiftableTokens = context.ShiftTokens
              message = context.Message }

        raise (GslParseError(newContext))

    ///<summary>
    /// Perform some selective deduplication of warnings.
    /// For now we just deduplicate DeprecationWarnings to only present them once.
    ///</summary>
    let deduplicateMessages (msgs: AstMessage list): AstMessage list =
        let depWarnings, others =
            msgs
            |> List.partition (fun msg ->
                match msg.Type with
                | AstMessageType.DeprecationWarning -> true
                | _ -> false)

        let dedupedDepWarnings =
            depWarnings
            |> List.distinctBy (fun dw -> dw.Message)
            |> List.map (fun dw ->
                { dw with
                      Message = sprintf "%s\nThis message will appear only once per file." dw.Message })

        dedupedDepWarnings @ others
