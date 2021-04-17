# Set context
biodb::testContext("Logger setting")

# Silence main logger
lgr::lgr$remove_appender(1)

# Set appender for biodb logger
logFile <- file.path(getwd(), '..', '..', 'biodb_tests.log')
layoutFormat <- lgr::LayoutFormat$new("%g.%L[%t][%c] %m")
app <- lgr::AppenderFile$new(logFile, layout=layoutFormat)
biodb::getLogger()$add_appender(app, "biodb.test")
