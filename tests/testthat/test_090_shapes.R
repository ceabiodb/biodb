# vi: fdm=marker

source('common.R', local=TRUE)
biodb <- biodb::createBiodbTestInstance()
obs <- biodb::addMsgRecObs(biodb)

# Set context
biodb::setTestContext(biodb, "Test shapes.")

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

# Main {{{1
################################################################

biodb::testThat("We can create a rectangle object.", test.rect, biodb = biodb)
biodb::testThat("We can create a circle object.", test.circle, biodb = biodb)

# Terminate Biodb
biodb$terminate()
