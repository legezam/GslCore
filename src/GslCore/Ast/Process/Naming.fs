namespace GslCore.Ast.Process.Naming

open Amyris.Dna
open GslCore.Ast.Process.ParsePart
open GslCore.Constants
open GslCore.Ast.Process
open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.GslResult
open GslCore.Pragma
open GslCore.Reference

[<RequireQualifiedAccess>]
type NameCheckError =
    | UnknownGene of geneName: string * basePart: AstNode
    | ReferenceError of errorMessage: string * node: AstNode
    | GetPragmaError of GetPragmaError

[<RequireQualifiedAccess>]
type NamingError =
    | AddPragma of PragmaArgumentError * node: AstNode
    | GetPragma of GetPragmaError


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

            ParsePart.getPragmasStrict parsePart
            |> GslResult.mapError NameCheckError.GetPragmaError
            >>= fun partPragmas ->

                    GenomeDefinitions.getReferenceGenome references [ partPragmas; assemblyPragmas ]
                    |> GslResult.fromResult (fun message -> NameCheckError.ReferenceError(message, node))
                    >>= fun reference ->
                            if reference
                               |> GenomeDefinition.isValidFeature geneName
                               || library.ContainsKey(geneName) then
                                GslResult.ok ()
                            else
                                GslResult.err (NameCheckError.UnknownGene(geneName, parsePart.Value.BasePart))
        | _ -> GslResult.ok ()

    /// Check all the gene names in the context of a single assembly.
    let private checkGeneNamesInAssembly (reference: GenomeDefinitions)
                                         (library: Map<string, Dna>)
                                         (node: AstNode)
                                         : GslResult<unit, NameCheckError> =
        match node with
        | AssemblyPart (partWrapper, assemblyWrapper) ->
            ParsePart.getPragmasStrict partWrapper
            |> GslResult.mapError NameCheckError.GetPragmaError
            >>= fun assemblyPragmas ->
                    assemblyWrapper.Value
                    |> List.map (checkGeneName reference library assemblyPragmas)
                    |> GslResult.collectA
                    |> GslResult.ignore
        | _ -> GslResult.ok ()

    /// Validate all gene names.
    let checkGeneNames (reference: GenomeDefinitions)
                       (library: Map<string, Dna>)
                       : AstTreeHead -> GslResult<AstTreeHead, NameCheckError> =
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
    let private nameAssembly (node: AstNode): GslResult<AstNode, NamingError> =
        match node with
        | AssemblyPart (assemblyWrapper, _) ->
            ParsePart.getPragmasStrict assemblyWrapper
            |> GslResult.mapError NamingError.GetPragma
            >>= fun pragmas ->
                    if pragmas
                       |> PragmaCollection.contains BuiltIn.namePragmaDef then
                        GslResult.ok node // already named
                    else
                        let literal = AstNode.decompile node |> cleanHashName

                        let name =
                            literal
                                .Substring(0, min literal.Length Default.NameMaxLength)
                                .Replace("@", "(@)")

                        let namePragma =
                            { Pragma.Definition = BuiltIn.namePragmaDef
                              Arguments = [ name ] }


                        pragmas
                        |> PragmaCollection.add namePragma
                        |> GslResult.mapError (fun error -> NamingError.AddPragma(error, node))
                        |> GslResult.map (fun mergedPragmas ->

                            let namedAssembly =
                                AstNode.Part(ParsePart.replacePragmas assemblyWrapper mergedPragmas)

                            let pragmaNode = AstNode.Pragma(Node.wrapNode namePragma)
                            AstNode.Block(Utils.nodeWrapWithNodePosition node [ pragmaNode; namedAssembly ]))
        | _ -> GslResult.ok node


    ///<summary>
    /// If an assembly does not have a name, generate one and stuff it in.
    /// Also prepend a name pragma, accomplished by replacing the assembly with a subblock that includes
    /// the new name pragma.
    ///</summary>
    let nameAssemblies (head: AstTreeHead): GslResult<AstTreeHead, NamingError> =
        FoldMap.map Serial TopDown nameAssembly head
