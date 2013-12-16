# Function for testing if a key exists inside a list/hashmap
hHasKey <- function(h, k) {
	return(length(which(names(h) == k)) > 0)
}

# Function for getting a boolean value from a list/hashmap
hGetBool <- function(h, k) {
	if (hHasKey(h, k)) return(h[[k]]) else return(FALSE)
}

