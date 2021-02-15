/// Functions used throughout GSLc that have no internal dependencies.
module GslCore.Utils

open System
open System.Reflection
open System.Text
open Amyris.Bio.primercore
open Microsoft.FSharp.Core.Printf
open Microsoft.FSharp.Reflection

/// Print an integer id that might not be assigned yet
let ambId (input: int option): string =
    match input with
    | None -> "?"
    | Some i -> string i

/// Produce a padding string of spaces n long
let pad (n: int): string = String.replicate n " "

let strToTempC (s: string): float<C> = (float s) * 1.0<C>

let limitTo n (s: string) =
    if s.Length > n then s.Substring(0, n) else s

///Returns the case name of the object with union type 'ty.
let getUnionCaseName (x: 'a) =
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

/// Boilerplate error to throw a meaningful exception if we screwed up a match expression that uses
/// a non-exhaustive combination of active patterns.
let nonExhaustiveError x =
    failwithf "A match expression with an active pattern had a non-exhaustive case: %A" x

/// Pretty print an exception chain, including all inner exceptions.
/// Uses reflection to print the type of each exception as well.
/// Taken from https://sergeytihon.wordpress.com/2013/04/08/f-exception-formatter/
// TODO: promote to Amyris.ErrorHandling
let prettyPrintException (e: Exception) =
    let builder = StringBuilder()
    let delimiter = String.replicate 50 "*"
    let newLine = Environment.NewLine

    let rec printException (e: Exception) depth =
        if (e :? TargetException && e.InnerException <> null) then
            printException (e.InnerException) depth
        else
            if depth = 1
            then bprintf builder "%s%s%s" e.Message newLine delimiter
            else bprintf builder "%s%s%d)%s%s%s" newLine newLine depth e.Message newLine delimiter

            bprintf builder "%sType: %s" newLine (e.GetType().FullName)
            // Loop through the public properties of the exception object
            // and record their values.
            e.GetType().GetProperties()
            |> Array.iter (fun p ->
                // Do not log information for the InnerException or StackTrace.
                // This information is captured later in the process.
                if (p.Name <> "InnerException"
                    && p.Name <> "StackTrace"
                    && p.Name <> "Message"
                    && p.Name <> "Data") then
                    try
                        let value = p.GetValue(e, null)

                        if (value <> null)
                        then bprintf builder "%s%s: %s" newLine p.Name (value.ToString())
                    with e2 -> bprintf builder "%s%s: %s" newLine p.Name e2.Message)

            if (e.StackTrace <> null) then
                bprintf builder "%s%sStackTrace%s%s%s" newLine newLine newLine delimiter newLine
                bprintf builder "%s%s" newLine e.StackTrace

            if (e.InnerException <> null)
            then printException e.InnerException (depth + 1)

    printException e 1
    builder.ToString()
