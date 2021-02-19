module GslCore.Core.AssemblyGathering

open GslCore.Ast.Types
open GslCore.Ast.Algorithms
open Amyris.ErrorHandling
open GslCore.Ast.LegacyParseTypes

// ===============================
// gathering every assembly in the tree for output generation
// ===============================

// because we need the pragma environment to perform assembly conversion, we need to be sneaky here
// we will use a closure that holds onto a mutable data structure by reference, and adds assemblies
// to it as it converts them during tree processing.

let private convertAndGatherAssembly (accum: ResizeArray<Assembly>) conversionContext node =
    match node with
    | AssemblyPart (pieces) ->
        convertAssembly conversionContext pieces
        >>= (fun convertedAssembly ->
            accum.Add(convertedAssembly)
            ok node) // pass the node through unchanged
    | _ -> ok node

/// Convert all assembly parts to legacy Assemblies, and gather them in a mutable accumulator.
/// Return the accumulation if all conversions were successful.
let convertAndGatherAssemblies tree =
    let accum = ResizeArray<Assembly>()

    let foldmapParameters =
        { FoldMapParameters.Direction = TopDown
          Mode = Serial
          StateUpdate = updateConversionContext
          Map = convertAndGatherAssembly accum }

    FoldMap.foldMap emptyConversionContext foldmapParameters tree
    >>= (fun treeOut ->
        // if successful, return the accumulated assemblies and the tree itself
        ok ((accum |> List.ofSeq), treeOut))
