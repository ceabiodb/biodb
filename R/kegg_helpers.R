# vi: fdm=marker

# Build KEGG pathway graphs {{{1
################################################################

build_kegg_pathway_graphs = function() {

	if (require('igraph')) {
		detach('package:igraph')
	}
}
