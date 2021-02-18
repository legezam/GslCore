module GslCore.BasicMarkerProvider

open GslCore.Constants
open GslCore.Ast.LegacyParseTypes
open GslCore.Core.Types
open Amyris.Dna
open GslCore.PluginTypes
open GslCore.DnaCreation

/// Default marker provider if nothing else better
let jobScorerBasicMarkerProvider _ = Some 0.0<PluginScore>

/// Expand a marker part into DNA pieces.
/// Exception on failure.
let expandMarkerPartIntoURA3 dnaSource (dna: Dna) (ppp: PPP) =

    { id = None
      extId = None
      sliceName = getSliceName ppp
      uri = getUri ppp // TODO: should this marker have a static URI we always assign here?
      dna = dna
      sourceChr = "library"
      sourceFr = 0<ZeroOffset>
      sourceTo = (dna.Length - 1) * 1<ZeroOffset>
      sourceFwd = true
      sourceFrApprox = false
      sourceToApprox = false
      // Don't assign coordinates to pieces until later when we
      // decide how they are getting joined up
      template = Some dna
      amplified = false
      destFr = 0<ZeroOffset>
      destTo = 0<ZeroOffset>
      destFwd = ppp.fwd
      description = "URA3 marker"
      sliceType = MARKER
      dnaSource = dnaSource
      pragmas = ppp.pr
      breed = B_MARKER
      materializedFrom = Some(ppp)
      annotations = [] }

/// Classic URA3 sequence that has always been hard coded into center of megastitches (from lib.fa)
let basicURA3 = "GTTCATCATCTCATGGATCTGCACATGAACAAACACCAGAGTCAAACGACGTTGAAATTG\
AGGCTACTGCGCCAATTGATGACAATACAGACGATGATAACAAACCGAAGTTATCTGATG\
TAGAAAAGGATTAAAGATGCTAAGAGATAGTGATGATATTTCATAAATAATGTAATTCTA\
TATATGTTAATTACCTTTTTTGCGAGGCATATTTATGGTGAAGGATAAGTTTTGACCATC\
AAAGAAGGTTAATGTGGCTGTGGTTTCAGGGTCCATAAAGCTTTTCAATTCATCTTTTTT\
TTTTTTGTTCTTTTTTTTGATTCCGGTTTCTTTGAAATTTTTTTGATTCGGTAATCTCCG\
AGCAGAAGGAAGAACGAAGGAAGGAGCACAGACTTAGATTGGTATATATACGCATATGTG\
GTGTTGAAGAAACATGAAATTGCCCAGTATTCTTAACCCAACTGCACAGAACAAAAACCT\
GCAGGAAACGAAGATAAATCATGTCGAAAGCTACATATAAGGAACGTGCTGCTACTCATC\
CTAGTCCTGTTGCTGCCAAGCTATTTAATATCATGCACGAAAAGCAAACAAACTTGTGTG\
CTTCATTGGATGTTCGTACCACCAAGGAATTACTGGAGTTAGTTGAAGCATTAGGTCCCA\
AAATTTGTTTACTAAAAACACATGTGGATATCTTGACTGATTTTTCCATGGAGGGCACAG\
TTAAGCCGCTAAAGGCATTATCCGCCAAGTACAATTTTTTACTCTTCGAAGACAGAAAAT\
TTGCTGACATTGGTAATACAGTCAAATTGCAGTACTCTGCGGGTGTATACAGAATAGCAG\
AATGGGCAGACATTACGAATGCACACGGTGTGGTGGGCCCAGGTATTGTTAGCGGTTTGA\
AGCAGGCGGCGGAAGAAGTAACAAAGGAACCTAGAGGCCTTTTGATGTTAGCAGAATTGT\
CATGCAAGGGCTCCCTAGCTACTGGAGAATATACTAAGGGTACTGTTGACATTGCGAAGA\
GCGACAAAGATTTTGTTATCGGCTTTATTGCTCAAAGAGACATGGGTGGAAGAGATGAAG\
GTTACGATTGGTTGATTATGACACCCGGTGTGGGTTTAGATGACAAGGGAGACGCATTGG\
GTCAACAGTATAGAACCGTGGATGATGTGGTCTCTACAGGATCTGACATTATTATTGTTG\
GAAGAGGACTATTTGCAAAGGGAAGGGATGCTAAGGTAGAGGGTGAACGTTACAGAAAAG\
CAGGCTGGGAAGCATATTTGAGAAGATGCGGCCAGCAAAACTAAAAAACTGTATTATAAG\
TAAATGCATGTATACTAAACTCACAAATTAGAGCTTCAATTTAATTATATCAGTTATTAC\
CCGGGAATCTCGGTCGTAATGATTTCTATAATGACGAAAAAAAAAAAATTGGAAAGAAAA\
AGCTTCATGGCCTTTATAAAAAGGAACTATCCAATACCTCGCCAGAACCAAGTAACAGTA"

type BasicURA3MarkerProvider =
    { ura3: Dna option }
    interface IMarkerProvider with
        member __.ProvidedArgs() = []
        member x.Configure(_) = x :> IMarkerProvider

        member x.ConfigureFromOptions(_opts) =
            { x with ura3 = Some(Dna(basicURA3)) } :> IMarkerProvider

        member x.Setup(_) = x :> IMarkerProvider

        member x.CreateDna(task: MarkerMaterializationTask) =
            expandMarkerPartIntoURA3 task.DnaSource x.ura3.Value task.PartPlusPragma

        member x.IsLegal m =
            m.ToLower() = "ura3" || m.ToLower() = "default"

        member x.ListMarkers() = [ "ura3" ]
        member x.ScoreJob = jobScorerBasicMarkerProvider

/// Original default URA3 behavior for materialized ### parts
let basicMarkerProviderURA3 =
    { Name = "classic ura3 dropin marker provider"
      Description = Some "Include default ura3 sequence in materialized ### sequences."
      Behaviors =
          [ { Name = None
              Description = None
              Behavior = MarkerProvider({ ura3 = None }) } ]
      ProvidesPragmas = []
      ProvidesCapas = [] }
