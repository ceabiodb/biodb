# Silence main logger
lgr::lgr$remove_appender(1)

# Set context
biodb::testContext("Logger setting")

# Log everything to file
logFile <- file.path(getwd(), '..', '..', 'biodb_tests.log')
layoutFormat <- lgr::LayoutFormat$new("%g.%L[%t][%c] %m")
app <- lgr::AppenderFile$new(logFile, layout=layoutFormat)
app$set_threshold('all')
lgr::lgr$add_appender(app, "biodb.test")
lgr::lgr$set_threshold('all')
