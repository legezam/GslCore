namespace GslCore.Legacy

open Amyris.Dna
open GslCore.Ast.Algorithms
open GslCore.Ast.Process
open GslCore.GslResult
open GslCore.Pragma
open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Process.AssemblyStuffing
open GslCore.DesignParams
open GslCore.Legacy.Types
open GslCore.PcrParamParse


type LegacyPartCreationError =
    | IllegalSliceConstruction of left: AstNode * right: AstNode
    | IllegalModifierNode of node: AstNode
    | IllegalLegacyBasePart of node: AstNode
    | IllegalPart of node: AstNode

type LegacyAssemblyCreationError =
    | DesignParamCreationError of DesignParameterError * AstNode
    | PartCreationError of LegacyPartCreationError

module LegacyAssemblyCreationError =
    let makeDesignParamCreationError (node: AstNode) (originalError: DesignParameterError) =
        DesignParamCreationError(originalError, node)

    let toAstMessage: LegacyAssemblyCreationError -> AstMessage =
        function
        | DesignParamCreationError (paramError, node) ->

            match paramError with
            | PcrParameterRevisionError err ->
                let message = err |> PcrParameterParseError.toString
                AstMessage.createErrorWithStackTrace PragmaError message node
        | PartCreationError partError ->
            match partError with
            | IllegalSliceConstruction (left, right) ->
                let context =
                    sprintf "legacy slice construction; found [%s:%s]" left.TypeName right.TypeName

                AstResult.internalTypeMismatchMsg (Some context) "RelPos" left
            | IllegalModifierNode node ->
                let context = Some "legacy mod conversion"
                AstResult.internalTypeMismatchMsg context "Slice or Mutation or DotMod" node
            | IllegalLegacyBasePart node ->
                let context = Some "legacy part conversion"
                AstResult.internalTypeMismatchMsg context "legacy-compatible base part" node
            | IllegalPart node ->
                let context = Some "legacy part conversion"
                AstResult.internalTypeMismatchMsg context "Part" node


// =====================
// conversion from AST parts to legacy parts
// =====================
module LegacyConversion =
    let private sliceFromAstSlice (parseSlice: ParseSlice): GslResult<Modifier, LegacyPartCreationError> =
        match parseSlice.Left, parseSlice.Right with
        | AstNode.RelPos leftWrapper, AstNode.RelPos rightWrapper ->
            GslResult.ok
                (Modifier.Slice
                    ({ Slice.Left = leftWrapper.Value
                       LeftApprox = parseSlice.LeftApprox
                       Right = rightWrapper.Value
                       RightApprox = parseSlice.RightApprox }))
        | x, y -> GslResult.err (IllegalSliceConstruction(x, y))

    let private astNodeToLegacyMod (node: AstNode): GslResult<Modifier, LegacyPartCreationError> =
        match node with
        | AstNode.Slice sw -> sliceFromAstSlice sw.Value
        | AstNode.Mutation mw -> GslResult.ok (Modifier.Mutation(mw.Value))
        | AstNode.DotMod dm -> GslResult.ok (Modifier.Dot(dm.Value))
        | _ -> GslResult.err (IllegalModifierNode node)

    let private convertModifiers (mods: AstNode list): GslResult<Modifier list, LegacyPartCreationError> =
        mods
        |> List.map astNodeToLegacyMod
        |> GslResult.collectA

    /// Convert an AST base part into a legacy Part.
    let private createLegacyPart (part: Node<ParsePart>): GslResult<Part, LegacyPartCreationError> =
        match part.Value.BasePart with
        | AstNode.Gene geneWrapper ->
            convertModifiers part.Value.Modifiers
            |> GslResult.map (fun mods ->
                let genePart =
                    { GenePart.Gene = geneWrapper.Value.Gene
                      Modifiers = mods
                      Where = geneWrapper.Positions }

                Part.GenePart
                    { GenePartWithLinker.Part = genePart
                      Linker = geneWrapper.Value.Linker })
        | AstNode.Marker _ -> GslResult.ok Part.MarkerPart
        | AstNode.InlineDna dnaSequence ->
            GslResult.ok (Part.InlineDna(Dna(dnaSequence.Value, true, AllowAmbiguousBases)))
        | AstNode.InlineProtein proteinSequence -> GslResult.ok (Part.InlineProtein proteinSequence.Value)
        | AstNode.HetBlock _ -> GslResult.ok Part.HeterologyBlock
        | AstNode.PartId partId ->
            convertModifiers part.Value.Modifiers
            |> GslResult.map (fun mods -> Part.PartId { Id = partId.Value; Modifiers = mods })
        | x -> GslResult.err (IllegalLegacyBasePart x)

    let private createPartPlusPragma (node: AstNode): GslResult<PartPlusPragma, LegacyPartCreationError> =
        match node with
        | AstNode.Part part ->
            createLegacyPart part
            |> GslResult.map (fun legacyPart ->
                { PartPlusPragma.Part = legacyPart
                  Pragma = ParsePart.getPragmas part
                  IsForward = part.Value.IsForward })
        | x -> GslResult.err (IllegalPart x)


    /// Accumulate both pragma and docstring context.
    let updateConversionContext (mode: StateUpdateMode)
                                (state: AssemblyConversionContext)
                                (node: AstNode)
                                : AssemblyConversionContext =
        let newPragmaEnv =
            AssemblyStuffing.updatePragmaEnvironment mode state.PragmaEnvironment node

        let newDocsEnv =
            Docstrings.updateDocstringEnvironment mode state.Docs node

        { state with
              PragmaEnvironment = newPragmaEnv
              Docs = newDocsEnv }

    /// Convert an AST assembly into a legacy assembly.
    let convertAssembly (context: AssemblyConversionContext)
                        (partWrapper: Node<ParsePart>, assemblyPartsWrapper: Node<AstNode list>)
                        : GslResult<Assembly, LegacyAssemblyCreationError> =
        let assemblyPragmas = ParsePart.getPragmas partWrapper

        let name =
            assemblyPragmas
            |> PragmaCollection.tryGetValue BuiltIn.namePragmaDef

        let uri =
            assemblyPragmas
            |> PragmaCollection.tryGetValue BuiltIn.uriPragmaDef

        let linkerHint =
            assemblyPragmas
            |> PragmaCollection.tryGetValues BuiltIn.linkersPragmaDef
            |> Option.map (String.concat "")
            |> Option.defaultValue ""


        let parameters =
            DesignParams.fromPragmas DesignParams.identity assemblyPragmas
            |> GslResult.mapError (LegacyAssemblyCreationError.makeDesignParamCreationError (AstNode.Part(partWrapper)))

        let parts =
            assemblyPartsWrapper.Value
            |> List.map createPartPlusPragma
            |> GslResult.collectA
            |> GslResult.mapError PartCreationError

        (parts, parameters)
        ||> GslResult.map2 (fun parts designParams ->
                { Assembly.Parts = parts
                  Name = name
                  Uri = uri
                  LinkerHint = linkerHint
                  Pragmas = assemblyPragmas
                  DesignParams = designParams
                  Capabilities = context.PragmaEnvironment.Capabilities
                  DocStrings = context.Docs.Assigned
                  SourcePosition = partWrapper.Positions })
