# vi: fdm=marker

# Run scheduler tests {{{1
################################################################

run.scheduler.tests <- function(biodb, obs) {

	set.test.context(biodb, "Test request scheduler")

	# TODO do some tests offline:
	# 1. Test that ChEBI connector create the right rule (Write some private methods inside scheduler to get info about rules).
	# 2. Test that frequency are updated correctly if another connector is created with different n and t (or same n and t).
	# 3. Test waiting time of scheduler. How?
}
