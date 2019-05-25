# vi: fdm=marker ts=4 et cc=80 

# Class declaration {{{1
################################################################

#' The connector class to KEGG Pathway database.
#'
#' This is a concrete connector class. It must never be instantiated directly,
#' but instead be instantiated through the factory \code{\link{BiodbFactory}}.
#' Only specific methods are described here. See super classes for the
#' description of inherited methods.
#'
#' @param id        A character vector of entry IDs.
#' @param drop      \code{TRUE} if you want to get a single element outside of
#'                  a list, or \code{FALSE} if you want to keep the element
#'                  inside a list even if it is single.
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
#' # Get a pathway graph
#' graph = conn$buildPathwayGraph('mmu00260')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggConn.R
#' @export KeggPathwayConn
#' @exportClass KeggPathwayConn
KeggPathwayConn <- methods::setRefClass("KeggPathwayConn",
                                        contains = c("KeggConn"))

# Public methods {{{1
################################################################################

# Constructor {{{2
################################################################

KeggPathwayConn$methods( initialize = function(...) {
    callSuper(db.name = 'pathway', db.abbrev = 'path', ...)
})

# Get entry image url {{{2
################################################################

KeggPathwayConn$methods( getEntryImageUrl = function(id) {
    return(rep(NA_character_, length(id)))
})

# Get reactions {{{2
################################################################

KeggPathwayConn$methods( getReactions = function(id, drop = TRUE) {
    "Retrieve all reactions part of a KEGG pathway. Connect to KEGG databases,
    and walk through all pathways submitted, and their modules, to find all
    reactions they are composed of. Returns a list of KEGG reaction objects or
    a single KEGG reaction objects, depending on the drop parameter."

    reactions = list()
    react_ids = character()
    
    kegg.mod.conn = .self$getBiodb()$getFactory()$getConn('kegg.module')
    
    # Loop on all Pathway IDs
    for (path.id in id) {
        
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
    reactions = kegg.react.conn$getEntry(react_ids, drop = FALSE)
    reactions = reactions[ ! vapply(reactions, is.null, FUN.VALUE = TRUE)]

    # Drop
    if (drop && length(reactions) <= 1)
        reactions = if (length(reactions) == 1) reactions[[1]] else NULL
 
    return(reactions)
})

# Convert to organism pathways {{{2
################################################################

KeggPathwayConn$methods( convertToOrgPathways = function(id, org) {
    "Take a list of pathways IDs and convert them to the specified organism,
    filtering out the ones that do not exist in KEGG."
    
    # Set organism code in IDs
    id <- sub('^[^0-9]+', org, id)
    
    # Get entries to check existence
    entries <- .self$getEntry(id, drop = FALSE)
    
    # Filter out non existing entries
    id <- id[ ! vapply(entries, is.null, FUN.VALUE = TRUE)]
    
    return(id)
})

# Build pathway graph {{{2
################################################################

KeggPathwayConn$methods( buildPathwayGraph = function(id, directed = FALSE,
                                                      drop = TRUE) {
    "Build a pathway graph using KEGG database. Returns a named list whose
    names are the pathway IDs, and values are lists containing two data frames
    named vertices and edges."

    graph = list()

    # Loop on all pathway IDs
    for (path.id in id) {

        edg = NULL
        vert = NULL

        # Loop on all reactions
        for (react in .self$getReactions(id)) {
            g <- .self$.buildReactionGraph(react, directed)
            if ( ! is.null(g)) {
                edg <- rbind(edg, g$edges)
                vert <- rbind(vert, g$vertices)
            }
        }
        
        # Build graph
        if ( ! is.null(edg)) {
            vert <- vert[ ! duplicated(vert[['name']]), ]
            graph[[path.id]] = list(vertices = vert, edges = edg)
        }
        else
            graph[[path.id]] = NULL
    }

    # Drop
    if (drop && length(graph) <= 1)
        graph = if (length(graph) == 1) graph[[1]] else NULL
    
    return(graph)
})

# Get pathway igraph {{{2
################################################################

KeggPathwayConn$methods( getPathwayIgraph = function(id, directed = FALSE,
                                                     drop = TRUE) {
    "Build a pathway graph using KEGG database. Returns an \\code{igraph}
    object, or NULL if the igraph library is not available."

    graph = list()
 
    if (require('igraph', quietly = TRUE, warn.conflicts = FALSE)) {
        detach('package:igraph') # Force using namespace.
        
        g = .self$buildPathwayGraph(id = id, directed = directed, drop = FALSE)
        for (n in names(g)) {
            
            # Get edges and vertices
            e = g[[n]][['edges']]
            v = g[[n]][['vertices']]
            
            # Set colors and shapes

            # Create igraph object
            graph[[n]] = igraph::graph_from_data_frame(e, directed = directed,
                                                       vertices = v)
        }
    }

    # Drop
    if (drop && length(graph) <= 1)
        graph = if (length(graph) == 1) graph[[1]] else NULL

    return(graph)
})

# Get decorated graph picture {{{2
################################################################

KeggPathwayConn$methods( getDecoratedGraphPicture = function(id, color2ids) {
 
    pix = NULL
    
    if (require('magick', quietly = TRUE, warn.conflicts = FALSE)) {
        detach('package:magick') # Force using namespace.
        
        # Get image
        pix = .self$.getPathwayImage(id)
        
        # Extract shapes
        shapes = .self$extractPathwayMapShapes(id = id, color2ids = color2ids)
        
        # Draw shapes
        dev = magick::image_draw(pix)
        for (shape in shapes)
            shape$draw()
        dev.off()
        pix = dev
    }
    
    return(pix)
})

# Extract shapes from pathway map {{{2
################################################################

KeggPathwayConn$methods( extractPathwayMapShapes = function(id, color2ids) {
                            
    shapes = list()
    
    html <- .self$.getPathwayHtml(id)
    
    for (color in names(color2ids)) {
        
        for (id in color2ids[[color]]) {
            
            # Escape special chars
            eid <- gsub('\\.', '\\\\.', id)
            
            regex =  paste0('shape=([^ ]+)\\s+',
                            'coords=([^ ]+)\\s+.+',
                            'title="[^"]*[ ,](', eid, ')[ ,][^"]*"')
            g = stringr::str_match_all(html, regex)[[1]]
            if (nrow(g) > 0) {
                
                for (i in 1:nrow(g)) {
                    
                    type <- g[i, 2]
                    c = as.integer(strsplit(g[i, 3], ',')[[1]])
                    s <- switch(type,
                                rect = BiodbRect(label = g[i, 4],
                                           color = color,
                                           left = c[[1]], top = c[[2]],
                                           right = c[[3]], bottom = c[[4]]),
                                circle = BiodbCircle(label = g[i, 4],
                                             color = color, x = c[[1]],
                                             y = c[[2]], r = c[[3]]),
                                NULL)
                
                    # Append new shape to list
                    if ( ! is.null(s))
                        shapes = c(shapes, list(s))
                }
            }
        }
    }
        
    return(shapes)
})

# Private methods {{{1
################################################################################

# Get pathway HTML page {{{2
################################################################################

KeggPathwayConn$methods( .getPathwayHtml = function(id) {

    # Extract pathway number
    path_idx <- sub('^[^0-9]+', '', id)

    # Build Request
    url = BiodbUrl(url = c(.self$getPropValSlot('urls', 'base.url'), 'kegg-bin',
                           'show_pathway'),
                   params = c(org_name = 'map', mapno = path_idx,
                              mapscale = '1.0', show_description = 'hide'))
    request = BiodbRequest(url = url)

    # Send request and get HTML page
    html = .self$getBiodb()$getRequestScheduler()$sendRequest(request)
    
    return(html)
})

# Get pathway image {{{2
################################################################

KeggPathwayConn$methods( .getPathwayImage = function(id) {

    html <- .self$.getPathwayHtml(id)
    path_idx <- sub('^[^0-9]+', '', id)
    
    cache = .self$getBiodb()$getCache()
    img_filename = paste0('pathwaymap-', path_idx)
    img_file = cache$getFilePath(.self$getCacheId(), 'shortterm', img_filename, 'png')
    if ( ! cache$fileExist(.self$getCacheId(), 'shortterm', img_filename, 'png')) {
        img_url = stringr::str_match(html, 'src="([^"]+)"\\s+name="pathwayimage"')
        if (is.na(img_url[1, 1]))
            .self$message('error', 'Impossible to find pathway image path inside HTML page.')
        img_url = BiodbUrl(url = c(.self$getPropValSlot('urls', 'base.url'), img_url[1, 2]))
        .self$getBiodb()$getRequestScheduler()$downloadFile(url = img_url, dest.file = img_file)
    }
    
    return(magick::image_read(img_file))
})

# Build reaction graph {{{2
################################################################

KeggPathwayConn$methods( .buildReactionGraph = function(react, directed) {

    graph <- NULL
    
    # Get substrates and products
    subst = react$getFieldValue('substrates')
    prod = react$getFieldValue('products')
    
    if ( ! is.null(subst) && ! is.null(prod)) {

        # Create compound vertices
        ids <- c(subst, prod)
        vert = data.frame(name = ids, type = 'compound', id = ids)
        
        # Create reaction edge
        rid = react$getFieldValue('accession')
        vert = rbind(vert,
                     data.frame(name = rid, type = 'reaction', id = rid))
        edg = rbind(data.frame(from = subst, to = rid),
                    data.frame(from = rid, to = prod))
        
        # Create reverse reaction edge
        if (directed) {
            rvid = paste(rid, 'rev', sep = '_')
            vert = rbind(vert,
                         data.frame(name = rvid, type = 'reaction',
                                    id = rid))
            edg = rbind(edg, data.frame(from = prod, to = rvid),
                        data.frame(from = rvid, to = subst))
        }
        
        graph = list(edges = edg, vertices = vert)
    }
    
    return(graph)
})
