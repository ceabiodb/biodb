#######################
# WHITESPACE TRIMMING #
#######################

# Trim leading whitespaces
trim.leading <- function (x)  sub("^\\s+", "", x)

# Trim trailing whitespaces
trim.trailing <- function (x) sub("\\s+$", "", x)

# Trim leading and trailing whitespaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

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
