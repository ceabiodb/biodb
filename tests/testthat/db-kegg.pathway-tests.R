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

# Run KEGG Pathway tests {{{1
################################################################

run.kegg.pathway.tests = function(conn, obs) {

    if (test.online()) {
        test.that('getReactions() works correctly.',
                  'test_kegg_pathway_getReactions', conn = conn)
        test.that('buildPathwayGraph() works correctly.',
                  'test_kegg_pathway_buildPathwayGraph', conn = conn)
    }
}
