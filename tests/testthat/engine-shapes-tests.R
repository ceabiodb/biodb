# vi: fdm=marker

# Test rect {{{1
################################################################

test.rect <- function(biodb) {

	rect = BiodbRect(label = 'A', color = 'red',
		left = 10L, right = 20L, top = 5L, bottom = 40L)
	expect_is(rect, 'BiodbRect')
}

# Test circle {{{1
################################################################

test.circle <- function(biodb) {

	circle = BiodbCircle(label = 'A', color = 'red',
		x = 10L, y = 20L, r = 5L)
	expect_is(circle, 'BiodbCircle')
}

# Run shapes tests {{{1
################################################################

run.shapes.tests <- function(biodb) {

	set.test.context(biodb, "Test shapes")

	test.that("We can create a rectangle object.", 'test.rect', biodb = biodb)
	test.that("We can create a circle object.", 'test.circle', biodb = biodb)
}
