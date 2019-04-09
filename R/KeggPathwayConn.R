# vi: fdm=marker ts=4 et cc=80 

# Constants {{{1
################################################################

.BIODB.KEGG.PATHWAY.PARSING.EXPR <- list(
    'accession'              = "^ENTRY\\s+(\\S+)\\s+Pathway"
)

# Class declaration {{{1
################################################################

#' The connector class to KEGG Pathway database.
#'
#' This is a concrete connector class. It must never be instantiated directly, but instead be instantiated through the factory \code{\link{BiodbFactory}}. Only specific methods are described here. See super classes for the description of inherited methods.
#'
#' @param id        A character vector of entry IDs.
#' @param drop      \code{TRUE} if you want to get a single element outside of a list, or \code{FALSE} if you want to keep the element inside a list even if it is single.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{KeggConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get connector
#' conn = mybiodb$getFactory()$createConn('kegg.pathway')
#'
#' # Retrieve all reactions related to a mouse pathway:
#' reactions = conn$getReactions('mmu00260')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggConn.R
#' @export KeggPathwayConn
#' @exportClass KeggPathwayConn
KeggPathwayConn <- methods::setRefClass("KeggPathwayConn", contains = c("KeggConn"))

# Constructor {{{1
################################################################

KeggPathwayConn$methods( initialize = function(...) {
    callSuper(db.name = 'pathway', db.abbrev = 'path', ...)
})

# Get entry image url {{{1
################################################################

KeggPathwayConn$methods( getEntryImageUrl = function(id) {
    return(rep(NA_character_, length(id)))
})

# Get reactions {{{1
################################################################

KeggPathwayConn$methods( getReactions = function(pathway.id, drop = TRUE) {
    "Retrieve all reactions part of a KEGG pathway. Connect to KEGG databases, and walk through all pathways submitted, and their modules, to find all reactions they are composed of. Returns a list of KEGG reaction objects or a single KEGG reaction objects, depending on the drop parameter."

    reactions = list()
    react_ids = character()
    
    kegg.mod.conn   = .self$getBiodb()$getFactory()$getConn('kegg.module')
    
    # Loop on all Pathway IDs
    for (path.id in pathway.id) {
        
        path = .self$getEntry(path.id)
        if ( ! is.null(path) && path$hasField('kegg.module.id')) {
            
            # Loop on all modules
            for (mod.id in path$getFieldValue('kegg.module.id')) {
                
                mod = kegg.mod.conn$getEntry(mod.id)
                if ( ! is.null(mod) && mod$hasField('kegg.reaction.id'))
                    
                    react_ids = c(react_ids,
                                  mod$getFieldValue('kegg.reaction.id'))
            }
        }
    }
    
    react_ids = unique(react_ids)
    
    kegg.react.conn = .self$getBiodb()$getFactory()$getConn('kegg.reaction')
    reactions = kegg.react.conn$getEntry(react_ids)
    reactions = reactions[ ! vapply(reactions, ! is.null, FUN.VALUE = TRUE)]

    return(reactions)
}

# Build pathway graph {{{1
################################################################

KeggPathwayConn$methods( buildPathwayGraph = function(id) {
    "Build a pathway graph using KEGG database. Returns an \\code{igraph} object, or NULL if the igraph library is not available."

    graph = NULL
    
    if (require('igraph')) {
        detach('package:igraph') # Force using namespace.

        edges = NULL
        vertices = NULL

        # Loop on all reactions
        for (react in .self$getReactions(pathway.id)) {
            
            substrates = react$getFieldValue('substrates')
            products = react$getFieldValue('products')

            if (is.null(substrates) || is.null(products))
                next

            # Create vertices
            react_vertex_id = paste(react$getFieldValue('accession'), dir, sep = '_')
            vertices = rbind(vertices, data.frame(name = react_vertex_id, type = 'reaction'))
            for (c in c(substrates, products))
                if ( ! c %in% vertices[['name']])
                    vertices = rbind(vertices, data.frame(name = c, type = 'compound'))
            
            # Set edges
            for (dir in c(1, 2)) {

                # Reverse substrates/products
                if (dir == 2) {
                    tmp = substrates
                    substrates = products
                    products = tmp
                }

                # Create links to substrates
                for (substrate in substrates)
                    edges = rbind(edges, data.frame(from = substrate, to = react_vertex_id))

                # Create links to products
                for (product in products)
                    edges = rbind(edges, data.frame(from = react_vertex_id, to = product))
            }
        }
        
        # Build graph
        if ( ! is.null(edges) && ! is.null(vertices))
            graph = igraph::graph_from_data_frame(edges, vertices = vertices)
    }
    
    return(graph)
}

# Private methods {{{1
################################################################

# Get parsing expressions {{{2
################################################################

KeggPathwayConn$methods( .getParsingExpressions = function() {
    return(.BIODB.KEGG.PATHWAY.PARSING.EXPR)
})
