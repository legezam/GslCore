namespace GslCore.GslResult

open FsToolkit.ErrorHandling

// ================
// error handling support
// ================

[<Struct>]
type Success<'a, 'b> = { Result: 'a; Warnings: 'b list }

module Success =
    let inline combine (first: Success<'a, 'b>) (second: Success<'a, 'b>): Success<'a list, 'b> =
        { Success.Result = first.Result :: second.Result :: []
          Warnings = first.Warnings @ second.Warnings }

    let inline append (appendTo: Success<'a list, 'b>) (first: Success<'a, 'b>): Success<'a list, 'b> =
        { Result = appendTo.Result @ [ first.Result ]
          Warnings = first.Warnings @ appendTo.Warnings }

    let inline create (result: 'a): Success<'a, 'b> =
        { Success.Result = result
          Warnings = [] }

    let inline withWarning (warning: 'b) (this: Success<'a, 'b>) =
        { this with
              Warnings = this.Warnings @ [ warning ] }

    let inline withWarnings (warnings: 'b list) (this: Success<'a, 'b>): Success<'a, 'b> =
        { this with
              Warnings = this.Warnings @ warnings }


[<Struct>]
type GslResult<'a, 'b> =
    | GslResult of Result<Success<'a, 'b>, 'b list>
    member this.Value =
        let (GslResult value) = this
        value

    static member inline Create(input: Result<Success<'a, 'b>, 'b list>) = GslResult input

    static member inline GetValue(this: GslResult<'a, 'b>) =
        let (GslResult result) = this
        result

module GslResult =
    let inline warn msg result: GslResult<'a, 'b> =
        result
        |> Success.create
        |> Success.withWarning msg
        |> Ok
        |> GslResult.Create

    let inline warns msgs result: GslResult<'a, 'b> =
        result
        |> Success.create
        |> Success.withWarnings msgs
        |> Ok
        |> GslResult.Create

    let inline ok result: GslResult<'a, 'b> =
        result |> Success.create |> Ok |> GslResult.Create

    let inline err (msg: 'b): GslResult<'a, 'b> = Result.Error [ msg ] |> GslResult.Create

    let inline collectA (results: GslResult<'a, 'b> list): GslResult<'a list, 'b> =
        results
        |> List.map GslResult.GetValue
        |> List.sequenceResultA
        |> Result.mapError (List.collect id)
        |> Result.map (fun results ->
            results
            |> List.fold Success.append (Success.create []))
        |> GslResult.Create

    let inline collectM (results: GslResult<'a, 'b> list): GslResult<'a list, 'b> =

        results
        |> List.map GslResult.GetValue
        |> List.sequenceResultM
        |> Result.map (fun results ->
            results
            |> List.fold Success.append (Success.create []))
        |> GslResult.Create

    let inline map (op: 'a -> 'b) (result: GslResult<'a, 'c>): GslResult<'b, 'c> =
        result
        |> GslResult.GetValue
        |> Result.map (fun okay ->
            { Success.Result = op okay.Result
              Warnings = okay.Warnings })
        |> GslResult.Create

    let inline bind (op: 'a -> GslResult<'b, 'c>) (result: GslResult<'a, 'c>): GslResult<'b, 'c> =
        result
        |> GslResult.GetValue
        |> Result.bind (fun okay ->
            op okay.Result
            |> GslResult.GetValue
            |> Result.map (fun nextOkay ->
                { Success.Result = nextOkay.Result
                  Warnings = okay.Warnings @ nextOkay.Warnings }))
        |> GslResult.Create

    let inline map2 (op: 'a -> 'b -> 'c) (resultA: GslResult<'a, 'd>) (resultB: GslResult<'b, 'd>): GslResult<'c, 'd> =
        let a = resultA |> GslResult.GetValue
        let b = resultB |> GslResult.GetValue

        Result.map2 (fun a b ->
            { Success.Result = op a.Result b.Result
              Warnings = a.Warnings @ b.Warnings }) a b
        |> GslResult.Create

    let inline map3 (op: 'a -> 'b -> 'c -> 'd)
                    (resultA: GslResult<'a, 'e>)
                    (resultB: GslResult<'b, 'e>)
                    (resultC: GslResult<'c, 'e>)
                    : GslResult<'d, 'e> =
        let a = resultA |> GslResult.GetValue
        let b = resultB |> GslResult.GetValue
        let c = resultC |> GslResult.GetValue

        Result.map3 (fun a b c ->
            { Success.Result = op a.Result b.Result c.Result
              Warnings = a.Warnings @ b.Warnings @ c.Warnings }) a b c
        |> GslResult.Create

    let inline optionalResult (op: 'a -> GslResult<'b, 'c>) (input: 'a option): GslResult<'b option, 'c> =
        input
        |> Option.map (op >> (map Some))
        |> Option.defaultValue (ok None)

    let inline combineValidations (first: 'a -> GslResult<'b, 'd>)
                                  (second: 'a -> GslResult<'c, 'd>)
                                  (input: 'a)
                                  : GslResult<unit, 'd> =
        ((first input), (second input))
        ||> map2 (fun _ _ -> ())

    let inline addMessages (messages: 'b list) (result: GslResult<'a, 'b>): GslResult<'a, 'b> =
        match result.Value with
        | Ok success ->
            Ok
                { success with
                      Warnings = success.Warnings @ messages }
            |> GslResult.Create
        | Error errors ->
            Result.Error(errors @ messages)
            |> GslResult.Create

    let inline addMessageToError (msg: 'b) (result: GslResult<'a, 'b>): GslResult<'a, 'b> =
        result.Value
        |> Result.eitherMap id (fun errors -> errors @ [ msg ])
        |> GslResult.Create

    let inline fromResult (errorMapper: 'b -> 'c) (input: Result<'a, 'b>): GslResult<'a, 'c> =
        match input with
        | Ok result -> ok result
        | Error err ->
            Result.Error [ (errorMapper err) ]
            |> GslResult.Create

    let inline mapError (messageMapper: 'b -> 'c) (input: GslResult<'a, 'b>): GslResult<'a, 'c> =
        match input.Value with
        | Ok success ->
            { Success.Result = success.Result
              Warnings = success.Warnings |> List.map messageMapper }
            |> Ok
            |> GslResult.Create
        | Error messages ->
            messages
            |> List.map messageMapper
            |> Error
            |> GslResult.Create

    let inline mapErrors (errorMapper: 'b list -> 'c list) (input: GslResult<'a, 'b>): GslResult<'a, 'c> =
        match input.Value with
        | Ok success ->
            { Success.Result = success.Result
              Warnings = success.Warnings |> errorMapper }
            |> Ok
            |> GslResult.Create
        | Error messages ->
            messages
            |> errorMapper
            |> Error
            |> GslResult.Create


    let inline ignore (original: GslResult<'a, 'b>): GslResult<unit, 'b> =
        match original.Value with
        | Ok success ->
            Ok
                ({ Success.Result = ()
                   Warnings = success.Warnings })
            |> GslResult.Create
        | Error errors -> Result.Error errors |> GslResult.Create

    let inline promote (op: 'a -> 'b) (input: 'a): GslResult<'b, 'c> = ok input |> map op

    let inline mapMessages (op: 'b -> 'b) (result: GslResult<'a, 'b>) =
        match result.Value with
        | Ok success ->
            Ok
                ({ success with
                       Warnings = success.Warnings |> List.map op })
        | Error errors -> Result.Error(errors |> List.map op)
        |> GslResult.Create

    let inline valueOr (op: 'b list -> 'a) (input: GslResult<'a, 'b>): 'a =
        match input.Value with
        | Ok success -> success.Result
        | Error errors -> op errors

    let inline fold (folder: 'a -> 'c -> GslResult<'a, 'b>) (initialState: 'a) (list: 'c list): GslResult<'a, 'b> =

        let rec loop (currentStateResult: GslResult<'a, 'b>) (remaining: 'c list): GslResult<'a, 'b> =
            currentStateResult
            |> bind (fun currentState ->
                match remaining with
                | head :: tail ->
                    let updatedState = folder currentState head
                    loop updatedState tail
                | [] -> ok currentState)

        loop (ok initialState) list


[<AutoOpen>]
module Operators =
    let (>>=) a b = GslResult.bind b a

    let (>=>) a b = fun inp -> a inp >>= b

    let (&&&) = GslResult.combineValidations
