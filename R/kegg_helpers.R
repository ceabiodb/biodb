# vi: fdm=marker ts=4 et cc=80 

# Get KEGG pathway reactions {{{1
################################################################

#' Retrieve all reactions part of a KEGG pathway.
#'
#' Connect to KEGG databases, and walk through all pathways submitted, and
#' their modules, to find all reactions they are composed of.
#'
#' @param pathway.id A vector of KEGG pathway IDs.
#' @return A vector of KEGG reaction IDs.
#'
#' @examples
#' reactions = get_kegg_pathway_reactions('mmu00260')
#'
#' @export
get_kegg_pathway_reactions = function(pathway.id) {

    reactions = character()
    
    biodb.ins = biodb::Biodb()
    kegg.path.conn  = biodb.ins$getFactory()$createConn('kegg.pathway')
    kegg.mod.conn   = biodb.ins$getFactory()$createConn('kegg.module')
    
    # Loop on all Pathway IDs
    for (path.id in pathway.id) {
        
        path = kegg.path.conn$getEntry(path.id)
        if ( ! is.null(path) && path$hasField('kegg.module.id')) {
            
            # Loop on all modules
            for (mod.id in path$getFieldValue('kegg.module.id')) {
                
                mod = kegg.mod.conn$getEntry(mod.id)
                if ( ! is.null(mod) && mod$hasField('kegg.reaction.id'))
                    
                    reactions = c(reactions,
                                  mod$getFieldValue('kegg.reaction.id'))
            }
        }
    }
    
    biodb.ins$terminate()
    
    reactions = unique(reactions)
    
    return(reactions)
}

# Build KEGG pathway graph {{{1
################################################################

#' Build a pathway graph using KEGG database.
#'
#'
#'
#' @param  pathway.id A vector of KEGG pathway IDs.
#' @return An \code{igraph} object.
#'
#' @examples
#' graph = build_kegg_pathway_graph('mmu00260')
#'
#' @export
build_kegg_pathway_graph = function(pathway.id) {

    graph = NULL
    
    if (require('igraph')) {
        detach('package:igraph') # Force using namespace.

        biodb.ins = biodb::Biodb()
        kegg.react.conn = biodb.ins$getFactory()$createConn('kegg.reaction')
        
        # Loop on all reactions
        for (react.id in get_kegg_pathway_reactions(pathway.id)) {
            
            react = kegg.react.conn$getEntry(react.id)
                
            # Form reaction vertex name
            react_vertex_id = react$getFieldValue('kegg.enzyme.id')
        }
        
        biodb.ins$terminate()
    }
    
    return(graph)
}

#' Get organism pathways.
#'
#' Take a list of KEGG compound IDs and look for pathways in which theses
#' compounds are involved for the specified organism.
#'
#' @param  comp.ids A list of KEGG compound IDs.
#' @param  org      The organism in which to search for pathways, as a KEGG
#'                  organism code (3-4 letters code, like "hsa", "mmu", ...).
#'                  See https://www.genome.jp/kegg/catalog/org_list.html for a
#'                  complete list of KEGG organism codes.
#' @return A vector of KEGG pathway IDs.
#'
#' @examples
#'
#' @export
get_kegg_pathways = function(comp.ids, org) {

    biodb.ins = biodb::Biodb()
    kegg.comp.conn = biodb.ins$getFactory()$createConn('kegg.compound')
    kegg.enz.conn  = biodb.ins$getFactory()$createConn('kegg.enzyme')
    kegg.gen.conn  = biodb.ins$getFactory()$createConn('kegg.genes')
    
    biodb.ins$terminate()
}
