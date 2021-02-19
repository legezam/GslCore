namespace GslCore.Ast.Process

open GslCore.Ast.Types
open GslCore.Ast.Algorithms

// ==================
// gathering and assigning docstrings
// ==================

/// Keep track of accumulated docstrings using a similar assignment system as we use for pragmas.
type DocstringEnvironment =
    { Unassigned: string list
      Assigned: string list }

module DocstringEnvironment =
    let empty = { Unassigned = []; Assigned = [] }

module Docstrings =
    /// Accumulate docstrings and assign them to assemblies.
    /// This function is only used during conversion to legacy assemblies.
    /// We might need to make this a bit more sophisticated to correctly ignore docstrings that
    /// are just kind of floating in the document that should be ignored.
    let updateDocstringEnvironmentInner (state: DocstringEnvironment) (node: AstNode): DocstringEnvironment =
        match node with
        | Docstring docStringWrapper ->
            { state with
                  Unassigned = docStringWrapper.Value :: state.Unassigned }
        | Part _ -> // assign these docs to this node, need to reverse the list
            { state with
                  Assigned = List.rev state.Unassigned
                  Unassigned = [] }
        | _ -> state

    let updateDocstringEnvironment =
        StateUpdateMode.pretransformOnly updateDocstringEnvironmentInner