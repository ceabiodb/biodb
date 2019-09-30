# vi: fdm=marker

source('common.R', local=TRUE)

# MAIN {{{1
################################################################

# Create biodb instance
biodb <- create.biodb.instance()
expect_is(biodb, 'Biodb')
obs <- add_msg_recorder_obs(biodb)

# Set context
biodb::setTestContext(biodb, "Test engine")

# Source all engine test files
for (f in Sys.glob('engine-*-tests.R'))
	source(f, local=TRUE)

# Terminate Biodb
biodb$terminate()
