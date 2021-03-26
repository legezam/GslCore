namespace GslCore.Ast.Process.Naming

open Amyris.Dna
open GslCore.Constants
open GslCore.Ast.Process
open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.GslResult
open GslCore.Pragma
open GslCore.Reference

type NameCheckError =
    | UnknownGene of geneName: string * basePart: AstNode
    | ReferenceError of errorMessage: string * node: AstNode

module NameCheckError =
    let toAstMessage: NameCheckError -> AstMessage =
        function
        | UnknownGene (geneName, basePart) -> AstResult.errStringFMsg AstMessageType.PartError "Unknown gene: '%s'." geneName basePart
        | ReferenceError (message, node) -> AstMessage.createErrorWithStackTrace AstMessageType.RefGenomeError message node


// ==================
// checking gene naming
// ==================
module NameChecking =
    /// If a node is a part with a gene, validate the name of that gene.
    /// Uses the pragmas of the enclosing part and the outer assembly context.
    let private checkGeneName (references: GenomeDefinitions)
                              (library: Map<string, Dna>)
                              (assemblyPragmas: PragmaCollection)
                              (node: AstNode)
                              : GslResult<unit, NameCheckError> =
        match node with
        | GenePart (parsePart, genePart) ->
            let geneName = genePart.Value.Gene.[1..].ToUpper()
            let partPragmas = ParsePart.getPragmas parsePart


            GenomeDefinitions.getReferenceGenome references [ partPragmas; assemblyPragmas ]
            |> GslResult.fromResult (fun message -> ReferenceError(message, node))
            >>= fun reference ->
                    if reference
                       |> GenomeDefinition.isValidFeature geneName
                       || library.ContainsKey(geneName) then
                        GslResult.ok ()
                    else
                        GslResult.err (UnknownGene(geneName, parsePart.Value.BasePart))
        | _ -> GslResult.ok ()

    /// Check all the gene names in the context of a single assembly.
    let private checkGeneNamesInAssembly (reference: GenomeDefinitions)
                                         (library: Map<string, Dna>)
                                         (node: AstNode)
                                         : GslResult<unit, NameCheckError> =
        match node with
        | AssemblyPart (partWrapper, assemblyWrapper) ->
            let assemblyPragmas = ParsePart.getPragmas partWrapper

            assemblyWrapper.Value
            |> List.map (checkGeneName reference library assemblyPragmas)
            |> GslResult.collectA
            |> GslResult.ignore
        | _ -> GslResult.ok ()

    /// Validate all gene names.
    let checkGeneNames (reference: GenomeDefinitions)
                       (library: Map<string, Dna>)
                       : AstTreeHead -> TreeTransformResult<NameCheckError> =
        Validation.validate (checkGeneNamesInAssembly reference library)


    // =====================
    // naming every assembly if it isn't named
    // =====================

    let private nameLegal =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789![]@$%^&*()'\":_-=+,.?/`~"
        |> Set.ofSeq

    let private isWhiteSpace: char -> bool =
        function
        | ' ' -> true
        | '\t' -> true
        | _ -> false

    let cleanHashName: string -> string =
        Seq.choose (fun character ->
            if nameLegal.Contains(character) then Some(character)
            else if isWhiteSpace character then None
            else Some('_'))
        >> Array.ofSeq
        >> Amyris.Bio.utils.arr2seq


    /// Name an assembly if it is not already named.
    /// We accomplish this by replacing the assembly with a subblock, into which we're placed a name pragma.
    /// Since naming happens after pragma stuffing, we also put the name pragma into the assebly itself.
    let private nameAssembly (node: AstNode): AstNode =
        match node with
        | AssemblyPart (assemblyWrapper, _) ->
            let pragmas = ParsePart.getPragmas assemblyWrapper

            if pragmas
               |> PragmaCollection.contains BuiltIn.namePragmaDef then node // already named
            else let literal = AstNode.decompile node |> cleanHashName

                 let name =
                     literal
                         .Substring(0, min literal.Length Default.NameMaxLength)
                         .Replace("@", "(@)")

                 let namePragma =
                     { Pragma.Definition = BuiltIn.namePragmaDef
                       Arguments = [ name ] }

                 let mergedPragmas =
                     pragmas |> PragmaCollection.add namePragma

                 let namedAssembly =
                     AstNode.Part(ParsePart.replacePragmas assemblyWrapper mergedPragmas)

                 let pragmaNode = AstNode.Pragma(Node.wrapNode namePragma)
                 AstNode.Block(Utils.nodeWrapWithNodePosition node [ pragmaNode; namedAssembly ])
        | _ -> node


    ///<summary>
    /// If an assembly does not have a name, generate one and stuff it in.
    /// Also prepend a name pragma, accomplished by replacing the assembly with a subblock that includes
    /// the new name pragma.
    ///</summary>
    let nameAssemblies (head: AstTreeHead): TreeTransformResult<'a> =
        FoldMap.map Serial TopDown (GslResult.promote nameAssembly) head
