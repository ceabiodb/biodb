BiodbObject <- methods::setRefClass("BiodbObject",
    fields=list(
                .message.enabled="logical"
                ),

methods=list(

initialize=function() {

    .self$.abstractClass('BiodbObject')

    .self$.message.enabled <- TRUE
},

getBiodb=function() {
    .self$abstract.method()
},

help=function() {
    utils::help(class(.self), 'biodb')
},

notify=function(fct, args) {

    # Get observers
    obs <- .self$getBiodb()$getObservers()

    # Build call code
    call <- paste0('do.call(o$', fct, ', args)')

    # Notify each observer
    lapply(obs, function(o) eval(parse(text=call)) )

    invisible()
},

progressMsg=function(msg, index, first, total=NA_integer_, type='info',
                     laptime=10L) {
    .self$notify('progress', list(type=type, msg=msg, index=index, total=total,
                                  first=first, laptime=laptime))

    invisible()
},

# Send a message to observers
message=function(type, msg, lvl=1) {

    type <- tolower(type)

    # Get biodb instance
    biodb <- NULL
    if (length(.self$.message.enabled) == 1 # length is 0
                                            # if called from constructor
        && .self$.message.enabled) {
        .self$.message.enabled <- FALSE
        biodb <- .self$getBiodb() 
        .self$.message.enabled <- TRUE
    }

    # Get class and method information
    class <- class(.self)
    method <- sys.call(length(sys.calls()) - 1)
    method <- sub('^[^$]*\\$([^(]*)(\\(.*)?$', '\\1()', method)[[1]]

    if ( ! is.null(biodb))
        .self$notify('msg', list(type=type, msg=msg, class=class, method=method,
                                 lvl=lvl))
    else {
        caller.info <- if (is.na(class)) '' else class
        if (! is.na(method))
            caller.info <- paste(caller.info, method, sep='::')
        if (nchar(caller.info) > 0)
            caller.info <- paste('[', caller.info, '] ', sep='')
        switch(type,
               error=stop(caller.info, msg),
               warning=warning(caller.info, msg),
               base::message(caller.info, msg))
    }

    invisible()
},

debug=function(...) {
    .self$message(type='debug', msg=paste0(...))

    invisible()
},

debug2=function(...) {
    .self$message(type='debug', msg=paste0(...), lvl=2)

    invisible()
},

debug2List=function(msg, lst, cut=10) {

    if (length(lst) == 0)
        s <- 'none'
    else {
        s <- paste(if (length(lst) > 10) c(lst[seq_len(10)], '...') else lst,
                   collapse=", ")
        s <- paste0('"', s, '"')
    }
    .self$debug2(msg, '[', length(lst), ']: ', s, '.')

    invisible()
},

error=function(...) {
    .self$message(type='error', msg=paste0(...))

    invisible()
},

warning=function(...) {
    .self$message(type='warning', msg=paste0(...))

    invisible()
},

caution=function(...) {
    .self$message(type='caution', msg=paste0(...))

    invisible()
},

info=function(...) {
    .self$message(type='info', msg=paste0(...))

    invisible()
},

info2=function(...) {
    .self$message(type='info', msg=paste0(...), lvl=2)

    invisible()
},

# This method is used to declare a class as abstract.
.abstractClass=function(class) {

    if (class == class(.self))
        .self$error('Class ', class, ' is abstract and thus cannot be ',
                    'instantiated.')
},

# This method is used to declare a method as abstract.
.abstractMethod=function() {

    class <- class(.self)
    method <- sys.calls()[[length(sys.calls()) - 1]]
    method <- as.character(method)
    method <- method[[1]]
    method <- sub('^[^$]*\\$([^(]*)(\\(.*)?$', '\\1()', method)

    .self$error("Method ", method, " is not implemented in ", class, " class.")
},

# This method is used to declare a method as deprecated.
.deprecatedMethod=function(new.method=NA_character_) {

    class <- class(.self)
    call <- sys.call(-1)
    call <- as.character(call)
    call <- call[[1]]
    calls <- strsplit(call, '$', fixed=TRUE)[[1]]
    method <- calls[[length(calls)]]

    msg <- paste("Method ", method, "() is now deprecated in ",
                 class, " class.", sep='')
    if ( ! is.na(new.method))
        msg <- paste(msg, " Please use now method ", new.method, ".", sep='')
    .self$message('caution', msg)
},

.assertNotNa=function(param, msg.type='error', sys.call.level=0,
                      param.name=NULL) {

    if (any(is.na(param))) {

        if (is.null(param.name))
            param.name <- as.character(sys.call(sys.call.level))[[2]]

        .self$message(msg.type, paste0(param.name, ' cannot be set to NA.'))

        return(FALSE)
    }

    return(TRUE)
},

.assertNotNull=function(param, msg.type='error', sys.call.level=0,
                            param.name=NULL) {

    if (is.null(param)) {

        if (is.null(param.name))
            param.name <- as.character(sys.call(sys.call.level))[[2]]

        .self$message(msg.type, paste(param.name, ' cannot be NULL.', sep=''))

        return(FALSE)
    }
    return(TRUE)
},

.assertInferior=function(param1, param2, msg.type='error',
                            na.allowed=TRUE) {

    # Remove NA values
    if (na.allowed) {
        nas <- is.na(param1) | is.na(param2)
        param1 <- param1[ ! nas]
        param2 <- param2[ ! nas]
    }

    # Compare
    if (any(param1 > param2)) {
        param1.name <- as.character(sys.call(0))[[2]]
        param2.name <- as.character(sys.call(0))[[3]]
        .self$message(msg.type, paste0(param1.name, ' (', param1,
                                       ') cannot be greater than ', param2.name,
                                       ' (', param2, ').'))
        return(FALSE)
    }

    return(TRUE)
},

.assertEqualLength=function(param1, param2, msg.type='error') {
    if (length(param1) != length(param2)) {
        param1.name <- as.character(sys.call(0))[[2]]
        param2.name <- as.character(sys.call(0))[[3]]
        .self$message(msg.type, paste0(param1.name, ' (length ', length(param1),
                                      ') has not the same length as ',
                                      param2.name, ' (length ', length(param2),
                                      ').'))
        return(FALSE)
    }
    return(TRUE)
},

.assertPositive=function(param, na.allowed=TRUE, zero=TRUE,
                            sys.call.level=0, msg.type='error',
                            param.name=NULL) {

    if (is.null(param.name))
        param.name <- as.character(sys.call(sys.call.level))[[2]]

    .self$.assertNotNull(param, msg.type=msg.type, param.name=param.name)
    if ( ! na.allowed)
        .self$.assertNotNa(param, msg.type=msg.type, param.name=param.name)

    if (any(param[ ! is.na(param)] < 0)
        || ( ! zero && any(param[ ! is.na(param)] == 0))) {
        .self$message(msg.type, paste0(param.name, ' (',
                                       paste(param, collapse=", "),
                                       ') cannot be negative',
                                       if (zero) '' else ' or equal to zero',
                                       '.'))
        return(FALSE)
    }

    return(TRUE)
},

.assertLengthOne=function(param, sys.call.level=0, msg.type='error',
                              param.name=NULL) {

    if (length(param) != 1) {

        if (is.null(param.name))
            param.name <- as.character(sys.call(sys.call.level))[[2]]

        .self$message(msg.type, paste0('Length of ', param.name,
                                      ' (', length(param), ') must be one.'))

        return(FALSE)
    }
    return(TRUE)
},

.assertIn=function(param, values, msg.type='error') {
    if ( ! is.na(param) && ! param %in% values) {
        param.name <- as.character(sys.call(0))[[2]]
        .self$message(msg.type, paste0(param.name, ' cannot be set to ', param,
                                      '. Allowed values are: ',
                                      paste(values, collapse=', '), '.'))
        return(FALSE)
    }
    return(TRUE)
},

.assertIs=function(param, type, sys.call.level=0, msg.type='error',
                      param.name=NULL) {

    if ( ! is.null(param) && ! class(param) %in% type) {

        if (is.null(param.name))
            param.name <- as.character(sys.call(sys.call.level))[[2]]

        .self$message(msg.type, paste0(param.name, ' is not of type ', type,
                                      ' but of type ', class(param), '.'))

        return(FALSE)
    }

    return(TRUE)
},

.assertInheritsFrom=function(param, super.class, msg.type='error') {
    if ( ! is.null(param) && ! is(param, super.class)) {
        param.name <- as.character(sys.call(0))[[2]]
        .self$message(msg.type, paste0(param.name, ' does not inherit from ',
                                       super.class, '.'))
        return(FALSE)
    }
    return(TRUE)
},

.assertNumber=function(param, length.one=TRUE, null.allowed=FALSE,
                          na.allowed=FALSE, zero=TRUE, negative=TRUE,
                          sys.call.level=0, integer.allowed=TRUE,
                          float.allowed=TRUE) {

    param.name <- as.character(sys.call(sys.call.level))[[2]]

    if ( ! null.allowed)
        .self$.assertNotNull(param, param.name=param.name)

    if ( ! is.null(param)) {

        if ( ! na.allowed)
            .self$.assertNotNa(param, param.name=param.name)

        types <- character()
        if (integer.allowed)
            types <- c(types, 'integer')
        if (float.allowed)
            types <- c(types, 'numeric')
        .self$.assertIs(param, types, param.name=param.name)

        if (length.one)
            .self$.assertLengthOne(param, param.name=param.name)

        if ( ! negative)
            .self$.assertPositive(param, na.allowed=na.allowed, zero=zero,
                                   param.name=param.name)
    }
}

))
