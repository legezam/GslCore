module GslCore.Core.AssemblyGathering

open GslCore.Ast.ErrorHandling
open GslCore.Ast.Types
open GslCore.Ast.Algorithms
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
        |> AstResult.map (fun convertedAssembly ->
            accum.Add(convertedAssembly)
            node) // pass the node through unchanged
    | _ -> AstResult.ok node

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
    |> AstResult.map (fun treeOut ->
        // if successful, return the accumulated assemblies and the tree itself
        (accum |> List.ofSeq), treeOut)
