namespace GslCore.Core.AssemblyGathering

open GslCore.Ast.Process.AssemblyStuffing
open GslCore.Ast.Types
open GslCore.Ast.Algorithms
open GslCore.Legacy.Types
open GslCore.Legacy
open GslCore.GslResult

// ===============================
// gathering every assembly in the tree for output generation
// ===============================

module AssemblyGathering =
    // because we need the pragma environment to perform assembly conversion, we need to be sneaky here
    // we will use a closure that holds onto a mutable data structure by reference, and adds assemblies
    // to it as it converts them during tree processing.

    let private convertAndGatherAssembly (accum: ResizeArray<Assembly>) conversionContext node =
        match node with
        | AssemblyPart members ->
            LegacyConversion.convertAssembly conversionContext members
            |> GslResult.map (fun convertedAssembly ->
                accum.Add(convertedAssembly)
                node) // pass the node through unchanged
        | _ -> GslResult.ok node

    /// Convert all assembly parts to legacy Assemblies, and gather them in a mutable accumulator.
    /// Return the accumulation if all conversions were successful.
    let convertAndGatherAssemblies (astTreeHead: AstTreeHead)
                                   : GslResult<Assembly list * AstTreeHead, FoldMapError<LegacyAssemblyCreationError, PragmaEnvironmentError>> =
        let accum = ResizeArray<Assembly>()

        let parameters =
            { FoldMapParameters.Direction = TopDown
              Mode = Serial
              StateUpdate = LegacyConversion.updateConversionContext
              Map = convertAndGatherAssembly accum }

        FoldMap.foldMap AssemblyConversionContext.empty parameters astTreeHead
        |> GslResult.map (fun result ->
            // if successful, return the accumulated assemblies and the tree itself
            (accum |> List.ofSeq), result)
