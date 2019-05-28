# vi: fdm=marker

source('common.R', local=TRUE)

# MAIN {{{1
################################################################

# Create biodb instance
biodb = create.biodb.instance()
expect_is(biodb, 'Biodb')
obs = create.test.observer(biodb)

# Set context
set.test.context(biodb, "Test engine")

# List all engine test files
files <- Sys.glob('engine-*-tests.R')

# Loop on all engine test files
for (f in files)
	source(f, local=TRUE)

# Terminate Biodb
biodb$terminate()
