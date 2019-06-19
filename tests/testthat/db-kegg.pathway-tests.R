# vi: fdm=marker ts=4 et cc=80 tw=80

# Test KEGG Pathway getReactions() {{{1
################################################################

test_kegg_pathway_getReactions = function(conn) {
    reactions = conn$getReactions('mmu00260', drop = FALSE)
    testthat::expect_is(reactions, 'list')
    testthat::expect_true(length(reactions) > 0)
    react_type = vapply(reactions,
                        function(x) methods::is(x, 'KeggReactionEntry'),
                        FUN.VALUE = TRUE)
    testthat::expect_true(all(react_type))
}

# Test KEGG Pathway buildPathwayGraph() {{{1
################################################################

test_kegg_pathway_buildPathwayGraph = function(conn) {
    graph = conn$buildPathwayGraph('mmu00260')
    testthat::expect_is(graph, 'list')
    testthat::expect_equal(names(graph), c('vertices', 'edges'))
}

# Test KEGG Pathway getPathwayIgraph() {{{1
################################################################

test_kegg_pathway_getPathwayIgraph = function(conn) {
    graph = conn$getPathwayIgraph('mmu00260')
    testthat::expect_is(graph, 'igraph')
}

# Test KEGG Pathway getDecoratedGraphPicture() {{{1
################################################################

test_getDecoratedGraphPicture = function(conn) {
 
    c = list(yellow = c('4.2.1.22', '4.2.3.1'), green = c('C00101', 'C00168'))
    graph_pix = conn$getDecoratedGraphPicture('mmu00260', color2ids = c)
    if (require('magick')) {
        detach('package:magick') # Force using namespace.
        testthat::expect_is(graph_pix, 'magick-image')
        magick::image_write(graph_pix, path = file.path(OUTPUT.DIR, 'test_getDecoratedGraphPicture_image.png'), format = 'png')
    }
    else
        testthat::expect_null(graph_pix)
}

# Test KEGG Pathway getDecoratedGraphPicture() with a wrong compound {{{1
################################################################

test_getDecoratedGraphPicture_not_a_compound = function(conn) {
    
    c = list(red = 'not_a_compound')
    fn <- 'test_getDecoratedGraphPicture_not_a_compound_image.png'
    fp <- file.path(OUTPUT.DIR, fn)
    graph_pix = conn$getDecoratedGraphPicture('mmu00260', color2ids = c)
    if (require('magick')) {
        detach('package:magick') # Force using namespace.
        testthat::expect_is(graph_pix, 'magick-image')
        
        magick::image_write(graph_pix, path = fp, format = 'png')
    }
    else
        testthat::expect_null(graph_pix)
}

# Test getDecoratedGraphPicture() for right enzyme highlighting {{{1
################################################################################

test_getDecoratedGraphPicture_right_enzyme <- function(conn) {
    
    pw <- 'map00500'
    enzymes <- c('3.2.1.133' = 2, '3.2.1.1' = 2)

    for (enz in names(enzymes)) {
        color2ids <- c(red = enz)
        shapes <- conn$extractPathwayMapShapes(id = pw, color2ids = color2ids)
        labels <- vapply(shapes, function(x) x$getLabel(), FUN.VALUE = '')
        testthat::expect_length(shapes, enzymes[[enz]])
        testthat::expect_true(all(labels == enz))
    }
}

# Main {{{1
################################################################################

test.that('getReactions() works correctly.',
          'test_kegg_pathway_getReactions', conn = conn)
test.that('buildPathwayGraph() works correctly.',
          'test_kegg_pathway_buildPathwayGraph', conn = conn)
test.that('getPathwayIgraph() works correctly.',
          'test_kegg_pathway_getPathwayIgraph', conn = conn)
test.that('We can build a decorated pathway graph.',
          'test_getDecoratedGraphPicture', conn = conn)
test.that('getDecoratedGraphPicture() does not fail when called with
unexisting compounds.',
          'test_getDecoratedGraphPicture_not_a_compound',
          conn = conn)
test.that('The right enzymes are highlighted on KEGG map.',
          'test_getDecoratedGraphPicture_right_enzyme', conn = conn)
