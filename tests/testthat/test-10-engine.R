# vi: fdm=marker

context('Starting engine tests.')

source('common.R')

source('engine-abstract-tests.R')
source('engine-assertions-tests.R')
source('engine-config-tests.R')
source('engine-factory-tests.R')
source('engine-object-printing-tests.R')
source('engine-observers-tests.R')
source('engine-scheduler-tests.R')

# MAIN {{{1
################################################################

# Create biodb instance
biodb <- create.biodb.instance()
expect_is(biodb, 'Biodb')
obs <- create.test.observer(biodb)
set.mode(biodb, MODE.OFFLINE)

# Run tests
run.abstract.tests(biodb, obs)
run.assertions.tests(biodb, obs)
run.object.printing.tests(biodb)
run.observers.tests(biodb, obs)
run.config.tests(biodb)
run.factory.tests(biodb, obs)
run.scheduler.tests(biodb, obs)

# Terminate Biodb
biodb$terminate()
