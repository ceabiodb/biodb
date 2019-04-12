# vi: fdm=marker ts=4 et cc=80

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
    testthat::expect_is(graph, 'igraph')
}

# Test KEGG Pathway getDecoratedGraphPicture() {{{1
################################################################

test_kegg_pathway_getDecoratedGraphPicture = function(conn) {
    
    # Set colors
    c = list(yellow = c('4.2.1.22', '4.2.3.1'), green = c('C00101', 'C00168'))
    graph_pix = conn$getDecoratedGraphPicture('mmu00260', color2ids = c)
    if (require('magick')) {
        detach('package:magick') # Force using namespace.
        testthat::expect_is(graph_pix, 'magick-image')
        magick::image_write(graph_pix, path = file.path(OUTPUT.DIR, 'test_kegg_pathway_getDecoratedGraphPicture_image.png'), format = 'png')
    }
    else
        testthat::expect_null(graph_pix)
}

# Run KEGG Pathway tests {{{1
################################################################

run.kegg.pathway.tests = function(conn, obs) {

    if (test.online()) {
        test.that('getReactions() works correctly.',
                  'test_kegg_pathway_getReactions', conn = conn)
        test.that('buildPathwayGraph() works correctly.',
                  'test_kegg_pathway_buildPathwayGraph', conn = conn)
        test.that('We can build a decorated pathway graph,',
                  'test_kegg_pathway_getDecoratedGraphPicture', conn = conn)
    }
}
