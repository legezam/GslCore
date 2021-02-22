namespace GslCore.Ast.ErrorHandling

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
type AstMessageType =
    | Context // a message which should be interpreted as additional context for a previous message
    | Warning
    | DeprecationWarning // we deduplicate these to avoid inundating the user
    | Error // totally generic error, ideally be more specific
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

    /// Pretty-print a short summart of an AST message.
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
    let createWarning = create None Warning

    /// Create a message that collects a stack trace, with unspecified type.
    let createErrorWithStackTrace = create (Some(StackTrace()))

    // ------ creating error results ------
//    type AstResult<'a> = Result<'a * (AstMessage list), AstMessage list>
//    ///Create a error result from a string and a node.
//    let createError msgType msg node: AstResult<'a> =
//        let msg =
//            createErrorWithStackTrace msgType msg node
//
//        Result.Error [ msg ]
//
//    ///Create a error result from a format string, single value, and node.
//    let createErrorf msgType msgfmt fmtVal node =
//        createError msgType (sprintf msgfmt fmtVal) node


    let optionalContextStr: string option -> string =
        function
        | Some s -> sprintf " in %s" s
        | None -> ""


[<Struct>]
type Success<'a> =
    { Result: 'a
      Warnings: AstMessage list }

module Success =
    let combine (first: Success<'a>) (second: Success<'a>): Success<'a list> =
        { Result = first.Result :: second.Result :: []
          Warnings = first.Warnings @ second.Warnings }

    let append (appendTo: Success<'a list>) (first: Success<'a>): Success<'a list> =
        { Result = appendTo.Result @ [ first.Result ]
          Warnings = first.Warnings @ appendTo.Warnings }

    let create (result: 'a) =
        { Success.Result = result
          Warnings = [] }

    let withWarning (warning: AstMessage) (this: Success<'a>) =
        { this with
              Warnings = this.Warnings @ [ warning ] }

    let withWarnings (warning: AstMessage list) (this: Success<'a>) =
        { this with
              Warnings = this.Warnings @ warning }

[<Struct>]
type AstResult<'a> =
    | AstResult of Result<Success<'a>, AstMessage list>
    member this.Value =
        let (AstResult value) = this
        value

    static member Create(input: Result<Success<'a>, AstMessage list>) = AstResult input

    static member GetValue(this: AstResult<'a>) =
        let (AstResult result) = this
        result

module AstResult =
    let warn msg result: AstResult<'a> =
        result
        |> Success.create
        |> Success.withWarning msg
        |> Ok
        |> AstResult.Create

    let ok result: AstResult<'a> =
        result |> Success.create |> Ok |> AstResult.Create

    let err (msg: AstMessage): AstResult<'a> = Result.Error [ msg ] |> AstResult.Create

    let errString msgType msg node: AstResult<'a> =
        AstMessage.createErrorWithStackTrace msgType msg node
        |> err

    let errStringF msgType msgfmt fmtVal node: AstResult<'a> =
        AstMessage.createErrorWithStackTrace msgType (sprintf msgfmt fmtVal) node
        |> err

    let collectA (results: AstResult<'a> list): AstResult<'a list> =
        results
        |> List.map AstResult.GetValue
        |> List.sequenceResultA
        |> Result.mapError (List.collect id)
        |> Result.map (fun results ->
            results
            |> List.fold Success.append (Success.create []))
        |> AstResult.Create

    let collectM (results: AstResult<'a> list): AstResult<'a list> =
        results
        |> List.map AstResult.GetValue
        |> List.sequenceResultM
        |> Result.map (fun results ->
            results
            |> List.fold Success.append (Success.create []))
        |> AstResult.Create

    let map (op: 'a -> 'b) (result: AstResult<'a>): AstResult<'b> =
        result
        |> AstResult.GetValue
        |> Result.map (fun okay ->
            { Success.Result = op okay.Result
              Warnings = okay.Warnings })
        |> AstResult.Create

    let bind (op: 'a -> AstResult<'b>) (result: AstResult<'a>): AstResult<'b> =
        result
        |> AstResult.GetValue
        |> Result.bind (fun okay ->
            op okay.Result
            |> AstResult.GetValue
            |> Result.map (fun nextOkay ->
                { Success.Result = nextOkay.Result
                  Warnings = okay.Warnings @ nextOkay.Warnings }))
        |> AstResult.Create

    let map2 (op: 'a -> 'b -> 'c) (resultA: AstResult<'a>) (resultB: AstResult<'b>): AstResult<'c> =
        let a = resultA |> AstResult.GetValue
        let b = resultB |> AstResult.GetValue

        Result.map2 (fun a b ->
            { Success.Result = op a.Result b.Result
              Warnings = a.Warnings @ b.Warnings }) a b
        |> AstResult.Create

    let map3 (op: 'a -> 'b -> 'c -> 'd)
             (resultA: AstResult<'a>)
             (resultB: AstResult<'b>)
             (resultC: AstResult<'c>)
             : AstResult<'d> =
        let a = resultA |> AstResult.GetValue
        let b = resultB |> AstResult.GetValue
        let c = resultC |> AstResult.GetValue

        Result.map3 (fun a b c ->
            { Success.Result = op a.Result b.Result c.Result
              Warnings = a.Warnings @ b.Warnings @ c.Warnings }) a b c
        |> AstResult.Create

    let optionalResult (op: 'a -> AstResult<'b>) (input: 'a option): AstResult<'b option> =
        input
        |> Option.map (op >> (map Some))
        |> Option.defaultValue (ok None)

    let combineValidations (first: 'a -> AstResult<'b>) (second: 'a -> AstResult<'c>) (input: 'a): AstResult<unit> =
        ((first input), (second input))
        ||> map2 (fun _ _ -> ())

    let mergeMessages (messages: AstMessage list) (result: AstResult<'a>): AstResult<'a> =
        match result.Value with
        | Ok success ->
            Ok
                { success with
                      Warnings = success.Warnings @ messages }
            |> AstResult.Create
        | Result.Error errors ->
            Result.Error(errors @ messages)
            |> AstResult.Create

    let ofResult (errorMapper: 'b -> AstMessage) (input: Result<'a, 'b>): AstResult<'a> =
        match input with
        | Ok result -> ok result
        | Result.Error err ->
            Result.Error [ (errorMapper err) ]
            |> AstResult.Create

    let ignore (original: AstResult<'a>): AstResult<unit> =
        match original.Value with
        | Ok success ->
            Ok
                ({ Success.Result = ()
                   Warnings = success.Warnings })
            |> AstResult.Create
        | Result.Error errors -> Result.Error errors |> AstResult.Create

    let promote (op: 'a -> 'b) (input: 'a): AstResult<'b> = op input |> ok

    let mapMessages (op: AstMessage -> AstMessage) (result: AstResult<'a>) =
        match result.Value with
        | Ok success ->
            Ok
                ({ success with
                       Warnings = success.Warnings |> List.map op })
        | Result.Error errors -> Result.Error(errors |> List.map op)
        |> AstResult.Create

    let appendMessageToError msg (result: AstResult<'a>): AstResult<'a> =
        result.Value
        |> Result.eitherMap id (fun errors -> errors @ [ msg ])
        |> AstResult.Create

    ///Create an error representing a type mismatch resulting from a bugged GSL program.
    let variableTypeMismatch (variableName: string) (declaredType: 'a) (expectedType: 'b) (node: AstNode): AstResult<'c> =
        let message =
            sprintf
                "The variable %s has been inferred to have the type %O, but is required to have the type %O in this context."
                variableName
                declaredType
                expectedType

        errString TypeError message node


    ///<summary>
    ///Create an internal error representing a type mismatch.
    ///This is a common pattern when unpacking AST entities, and implies
    ///a bug in compiler logic rather than an error in parsed source code.
    ///</summary>
    let internalTypeMismatch (maybeContext: string option) (expectedType: string) (actualNode: AstNode): AstResult<'a> =
        let message =
            sprintf
                "Expected a '%s'%s, but got a '%s'"
                expectedType
                (AstMessage.optionalContextStr maybeContext)
                (actualNode.TypeName)

        errString (InternalError(TypeError)) message actualNode

    ///Create an internal error if we encounter a pragma that hasn't been built.
    let unbuiltPragmaError (context: string option) (name: string) (node: AstNode): AstResult<'a> =
        let message =
            sprintf "Found an unbuilt pragma%s: '%s'" (AstMessage.optionalContextStr context) name

        errString (InternalError(PragmaError)) message node

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
                if showStackTrace then yield this.StackTrace.ToString()
            }
            |> String.concat "\n"

[<AutoOpen>]
module Operators =
    let (>>=) a b = AstResult.bind b a

    let (>=>) a b = fun inp -> a inp >>= b

    let (&&&) = AstResult.combineValidations

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
                | DeprecationWarning -> true
                | _ -> false)

        let dedupedDepWarnings =
            depWarnings
            |> List.distinctBy (fun dw -> dw.Message)
            |> List.map (fun dw ->
                { dw with
                      Message = sprintf "%s\nThis message will appear only once per file." dw.Message })

        dedupedDepWarnings @ others
