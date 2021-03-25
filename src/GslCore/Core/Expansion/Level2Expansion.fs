namespace GslCore.Core.Expansion.Level2Expansion


open GslCore.Ast.Phase1
open GslCore.Ast.Process
open GslCore.Constants
open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.Core.Expansion.Bootstrapping
open GslCore.Legacy.Types
open GslCore.Legacy
open GslCore.Core.Expansion
open GslCore.GslResult
open GslCore.Pragma
open GslCore.Reference
open GslCore.Core.PluginTypes
open GslCore.Ast.Process.AssemblyStuffing

type Level2ExpansionError =

    | ExpansionException of exn * node: AstNode
    | L2LineCreationError of LegacyL2LineCreationError
    | BoostrapError of BootstrapError<Phase1Error>
    | AssemblyStuffingFailure of AssemblyStuffingError


// ==========================
// expanding L2 GSL
// ==========================
module Level2Expansion =
    /// Core expansion of a single L2 expression line.
    let private expandL2Expression (providers: L2Provider list)
                                   (rgs: GenomeDefinitions)
                                   (construct: L2Line)
                                   : GslSourceCode =
        let line = construct.L2Design
        let pragmas = construct.Pragmas
        let capabilities = construct.Capabilities
        // TODO:   ensuring key parts must be valid GSL parts e.g. can't titrate a non g part,  can't delete a non genome part

        //
        // Different styles of expansion
        //
        // Explicit locus
        ////////////////////////////////////
        // No package, just deletion
        // HO^                    =====>   uHO ; ### ; dHO
        //
        // Explicit deletion locus plus expressoin
        // HO^ ; pA > gB           ====>   uHO ; pA ; gB ; ### ; dHO
        //
        // Explicit locus with two or more genes
        // HO^ ; pA > gB ; pC > gD ====>   uHO ; pA ; gB ; ### ; !gD ; !pA ; dHO
        //
        // Titrations:
        ////////////////////////////////////
        // Titration of native gene
        // pA>gB                   =====>  uB ; ### ; pA ; ~oB (DS_G_CDS in thumper parlance)
        //
        //
        // Titration of native gene with additional expression constructs
        //
        // pA>gB ; pc>gD          ======> uB ; pC ; gD ; ### ; pA ; gD[1:~500]


        /// Stitch or megastitch; if neither, default to megastitch, and megastitch stomps stitch
        let megastitch =
            match PragmaCollection.assemblyMode pragmas with
            | Megastitch -> true
            | Stitch -> false

        /// Which reference genome are we using
        let refGenome' =
            match pragmas
                  |> PragmaCollection.tryGetValue BuiltIn.refGenomePragmaDef with
            // specifying a different reference genome implies a non standard
            // DNA source, so we can use that too (they can override with dnasrc)
            | Some rg -> rg.Trim([| ' '; '\t' |])
            | None -> Default.RefGenome

        /// Parameters to pass to specific L2 algorithm implementation
        let designParams =
            { L2DesignParams.IsMegastitch = megastitch
              ReferenceGenomes = rgs
              ReferenceGenome = refGenome'
              Line = line
              Pragmas = pragmas }

        /// List including lower-level GSL strings
        match line.L2Locus with
        | Some locusWithPrefix ->
            // Explicit locus case.  E.g.  gADH1^::pTDH3>mERG10
            // Choose provider
            let fn =
                providers
                |> List.choose (fun provider ->
                    match provider.JobScorer capabilities with
                    | None -> None
                    | Some (score) -> Some(score, provider.ExplicitLocusProvider))
                |> List.sortWith (fun (a, _) (b, _) -> compare b a)
                |> // sort largest first
                List.head
                |> snd

            fn locusWithPrefix designParams // returns string list
        | None ->
            // Implicit locus case.  E.g.  gADH1^::pTDH3>mERG10
            // Choose provider
            let fn =
                providers
                |> List.choose (fun provider ->
                    match provider.JobScorer capabilities with
                    | None -> None
                    | Some (score) -> Some(score, provider.ImplicitLocusProvider))
                |> List.sortWith (fun (a, _) (b, _) -> compare b a)
                |> // largest first
                List.head
                |> snd

            fn designParams // returns string list

    let validateNoAssemblyInL2Promoter (node: AstNode) =
        match node with
        | AstNode.L2Element e ->
            // if you see an L2 element, check if the promoter looks like an Assembly
            match e.Value.Promoter with
            | AssemblyPart _ -> AstResult.errString L2ExpansionError "Unsupported use of an Assembly." node
            | RecursivePart _ ->
                AstResult.errString
                    (InternalError L2ExpansionError)
                    "Unexpected recursive part definition in L2 promoter position."
                    node
            | _ -> Validation.good
        | _ -> Validation.good

    /// Expand all level 2 expressions.
    let expandLevel2 (parameters: Phase1Parameters)
                     (providers: L2Provider list)
                     (referenceGenomes: GenomeDefinitions)
                     (tree: AstTreeHead)
                     : GslResult<AstTreeHead, Level2ExpansionError> =

        let bootstrapExpandL2Expression pragmaContext node =
            /// Perform the expansion operation, capturing any exception as an error.
            let expandCaptureException construct =
                try
                    expandL2Expression providers referenceGenomes construct
                    |> GslResult.ok
                with e -> GslResult.err (ExpansionException(e, node))


            match node with
            | AstNode.L2Expression l2e ->
                LegacyL2Conversion.convertL2Line pragmaContext l2e
                |> GslResult.mapError L2LineCreationError
                >>= expandCaptureException
                >>= (Bootstrapping.bootstrapPhase1 parameters l2e.Positions
                     >> GslResult.mapError BoostrapError)
            | _ -> GslResult.ok node

        let foldMapParameters =
            { FoldMapParameters.Direction = TopDown
              Mode = Serial
              StateUpdate = UpdatePragmaEnvironment.update
              Map = bootstrapExpandL2Expression }

        FoldMap.foldMap  // run the bootstrapped expand operation
            PragmaEnvironment.empty
            foldMapParameters
            tree
        >>= Bootstrapping.healSplices
        >>= (AssemblyStuffing.stuffPragmasIntoAssemblies
             >> GslResult.mapError AssemblyStuffingFailure) // Bootstrapped assemblies need their pragma environment reinjected
