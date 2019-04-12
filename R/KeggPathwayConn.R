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

# Build pathway graph {{{1
################################################################

KeggPathwayConn$methods( buildPathwayGraph = function(id, drop = TRUE,
                                                      directed = FALSE) {
    "Build a pathway graph using KEGG database. Returns an \\code{igraph}
    object, or NULL if the igraph library is not available."

    graph = list()
 
    if (require('igraph')) {
        detach('package:igraph') # Force using namespace.

        edges = NULL
        vertices = NULL
        dirs = if (directed) c(1, 2) else 0

        # Loop on all pathway IDs
        for (path.id in id) {
            
            # Loop on all reactions
            for (react in .self$getReactions(id)) {

                substrates = react$getFieldValue('substrates')
                products = react$getFieldValue('products')

                if (is.null(substrates) || is.null(products))
                    next

                # Create compound vertices
                for (c in c(substrates, products))
                    if ( ! c %in% vertices[['name']])
                        vertices = rbind(vertices,
                                         data.frame(name = c,
                                                    type = 'compound',
                                                    id = c))
                
                # Set edges
                for (dir in dirs) {

                    # Create reaction vertex
                    rid = react$getFieldValue('accession')
                    if (dir != 0)
                        rvid = rid
                    else
                        rvid = paste(rid, dir, sep = '_')
                    vertices = rbind(vertices,
                                     data.frame(name = rvid,
                                                type = 'reaction',
                                                id = rid))
                
                    # Reverse substrates/products
                    if (dir == 2) {
                        tmp = substrates
                        substrates = products
                        products = tmp
                    }

                    # Create links to substrates
                    for (substrate in substrates)
                        edges = rbind(edges, data.frame(from = substrate,
                                                        to = rvid))

                    # Create links to products
                    for (product in products)
                        edges = rbind(edges, data.frame(from = rvid,
                                                        to = product))
                }
            }
        
            # Build graph
            if ( ! is.null(edges) && ! is.null(vertices)) {
                g = igraph::graph_from_data_frame(edges, vertices = vertices)
                graph[[path.id]] = g
            }
        }
    }

    # Drop
    if (drop && length(graph) <= 1)
        graph = if (length(graph) == 1) graph[[1]] else NULL

    return(graph)
})

# Get decorated graph picture {{{1
################################################################

KeggPathwayConn$methods( getDecoratedGraphPicture = function(id, color2ids) {
 
    pix = NULL
    
    if (require('magick')) {
        detach('package:magick') # Force using namespace.

        # Extract pathway number
        path_idx = sub('^[^0-9]+', '', id)
            
        # Build Request
        url = BiodbUrl(url = c(.self$getUrl('base.url'), 'kegg-bin', 'show_pathway'),
                       params = c(org_name = 'map', mapno = path_idx,
                                  mapscale = '1.0', show_description = 'hide'))
        request = BiodbRequest(url = url)
        
        # Send request and get HTML page
        html = .self$getBiodb()$getRequestScheduler()$sendRequest(request)
        
        # Get image
        pix = .self$.getPathwayImage(path_idx = path_idx, html = html)
        
        # Extract shapes
        shapes = .self$.extractPathwayMapShapes(html = html, color2ids = color2ids)
        
        # Draw shapes
        dev = magick::image_draw(pix)
        for (shape in shapes) {
            
            # Make color
            c = col2rgb(shape$color)
            c = rgb(c[1,], c[2,], c[3,], 127, maxColorValue = 255)
            if (shape$type == 'rect')
                rect(shape$left, shape$bottom, shape$right, shape$top,
                     col = c, border = NA)
            else if (shape$type == 'circle')
                symbols(x = shape$x, y = shape$y,
                        circles = shape$radius, bg = c,
                        add = TRUE, inches = FALSE)
        }
        dev.off()
        pix = dev
    }
    
    return(pix)
})

# Private methods {{{1
################################################################

# Extract shapes from pathway map {{{2
################################################################

KeggPathwayConn$methods( .extractPathwayMapShapes = function(html, color2ids) {
                            
    shapes = list()
    
    for (color in names(color2ids)) {
        
        for (id in color2ids[[color]]) {
            
            regex =  paste0('shape=([^ ]+)\\s+coords=([^ ]+)\\s+.+title="[^"]*',
                            id, '[^"]*"')
            g = stringr::str_match_all(html, regex)[[1]]
            if ( ! is.na(g[1, 1])) {
                
                for (i in 1:nrow(g)) {
                    
                    # Create shape
                    s = list(type = g[i, 2], id = id, color = color)
                    
                    # Set coordinates
                    c = strsplit(g[i, 3], ',')[[1]]
                    if (s$type == 'rect')
                        s = c(s, left = c[[1]], top = c[[2]],
                                 right = c[[3]], bottom = c[[4]])
                    
                    else if (s$type == 'circle')
                        s = c(s, x = c[[1]], y = c[[2]], radius = c[[3]])

                    else
                        next
                
                    # Append new shape to list
                    if ( ! is.null(s))
                        shapes = c(shapes, list(s))
                }
            }
        }
    }
        
    return(shapes)
})

# Get pathway image {{{2
################################################################

KeggPathwayConn$methods( .getPathwayImage = function(path_idx, html) {
                            
    cache = .self$getBiodb()$getCache()
    img_filename = paste0('pathwaymap-', path_idx)
    img_file = cache$getFilePath(.self$getCacheId(), 'shortterm', img_filename, 'png')
    if ( ! cache$fileExist(.self$getCacheId(), 'shortterm', img_filename, 'png')) {
        img_url = stringr::str_match(html, 'src="([^"]+)"\\s+name="pathwayimage"')
        if (is.na(img_url[1, 1]))
            .self$message('error', 'Impossible to find pathway image path inside HTML page.')
        img_url = BiodbUrl(url = c(.self$getUrl('base.url'), img_url[1, 2]))
        .self$getBiodb()$getRequestScheduler()$downloadFile(url = img_url, dest.file = img_file)
    }
    
    return(magick::image_read(img_file))
})

# Get parsing expressions {{{2
################################################################

KeggPathwayConn$methods( .getParsingExpressions = function() {
    return(.BIODB.KEGG.PATHWAY.PARSING.EXPR)
})
