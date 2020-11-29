biodbVignettes <- data.frame()
files <- Sys.glob('*.Rmd')
for (f in files) {
	name <- sub('^(.*)\\.Rmd', '\\1', f, perl=TRUE)
	firstLines <- readLines(f, n=20)
	title <- grep("^title:", firstLines, value=TRUE)
	title <- sub('^title: *"(.*)\\.?"$', '\\1', title, perl=TRUE)
	desc <- grep("%\\\\VignetteIndexEntry", firstLines, value=TRUE)
	desc <- sub('^.*VignetteIndexEntry{(.*)}.*$', '\\1', desc, perl=TRUE)
    html <- paste0(name, '.html')
    link <- paste0('[', title, '](', html, ')')
    biodbVignettes <- rbind(biodbVignettes, data.frame(name=name, title=title,
                                                       desc=desc, html=html,
                                                       link=link))
}

make_vignette_ref <- function(name) {
	cat(biodbVignettes[biodbVignettes$name == name, 'link', drop=TRUE])
}
