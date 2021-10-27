# Silence main logger
lgr::lgr$remove_appender(1)

# Set context
biodb::testContext("Logger setting")

logDst <- Sys.getenv('BIODB_LOG_DST')
layoutFormat <- lgr::LayoutFormat$new("%g.%L[%t][%c] %m")

# Log to console
if (logDst == 'console') {
    app <- lgr::AppenderConsole$new(layout=layoutFormat)
    
# Log to file
} else {
    logFile <- file.path(getwd(), '..', '..', 'biodb_tests.log')
    if (file.exists(logFile))
        unlink(logFile)
    app <- lgr::AppenderFile$new(logFile, layout=layoutFormat)
}
# Log everything
app$set_threshold('all')
lgr::lgr$add_appender(app, "biodb.test")
lgr::lgr$set_threshold('all')
