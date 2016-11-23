if ( ! exists('trim')) { # Do not load again if already loaded

	#######################
	# WHITESPACE TRIMMING #
	#######################
	
	# Trim leading whitespaces
	trim.leading <- function (x)  sub("^\\s+", "", x)
	
	# Trim trailing whitespaces
	trim.trailing <- function (x) sub("\\s+$", "", x)
	
	# Trim leading and trailing whitespaces
	trim <- function (x) gsub("^\\s+|\\s+$", "", x)
	
	#############
	# SPLITTING #
	#############
	
	# s         The string to split.
	# sep       The separator on which to split.
	# trim      Trim whitespaces for the resulting elements.
	# unlist    Unlist the result, So that for a single string (i.e.: s has length 1), it returns a vector of strings instead of a list of vectors of strings.
	# RETURN    A list of strings.
	split.str <- function(s, sep = ',', trim = TRUE, unlist = FALSE) {
		v <- strsplit(s, sep)
		if (trim) v <- lapply(v, trim)
		if (unlist) v <- unlist(v)
		return(v)
	}

	########################
	# SPLIT KEY/VALUE LIST #
	########################

	split.kv.list <- function(s, sep = ',', kvsep = '=') {

		# Split
		kvs <- strsplit(strsplit(s, sep)[[1]], kvsep)

		# Get keys
		k <- vapply(kvs, function(x) x[[1]], FUN.VALUE = '')
		v <- vapply(kvs, function(x) x[[2]], FUN.VALUE = '')

		# Set names
		names(v) <- k

		return(v)
	}

	#########################
	# CONCAT KEY/VALUE LIST #
	#########################

	concat.kv.list <- function(x, sep = ',', kvsep = '=') {

		k <- names(x)

		s = paste(paste(names(x), x, sep = kvsep), collapse = sep)

		return(s)
	}

	#################
	# REMOVE QUOTES #
	#################

	remove.quotes <- function(s) {
		return(sub('^["\']?([^\'"]*)["\']?$', '\\1', s, perl = TRUE))
	}

} # end of load safe guard
