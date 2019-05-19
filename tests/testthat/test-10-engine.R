# vi: fdm=marker

source('common.R')

source('engine-abstract-tests.R')
source('engine-assertions-tests.R')
source('engine-config-tests.R')
source('engine-biodb-tests.R')
source('engine-factory-tests.R')
source('engine-object-printing-tests.R')
source('engine-observers-tests.R')
source('engine-scheduler-tests.R')
source('engine-shapes-tests.R')

# MAIN {{{1
################################################################

# Create biodb instance
biodb = create.biodb.instance(offline = TRUE)
expect_is(biodb, 'Biodb')
obs = create.test.observer(biodb)

# Run tests
run.abstract.tests(biodb, obs)
run.assertions.tests(biodb, obs)
run.object.printing.tests(biodb)
run.observers.tests(biodb, obs)
run.config.tests(biodb)
run.biodb.tests(biodb, obs)
run.factory.tests(biodb, obs)
run.scheduler.tests(biodb)
run.shapes.tests(biodb)

# Terminate Biodb
biodb$terminate()
