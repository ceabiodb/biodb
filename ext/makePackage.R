######CREATING A PACKAGE REPSITORY FROM A HIERARCHY WITH SOURCES

if(!require("igraph")) stop("impossible to make a package.")
if(!require("roxygen2")) stop("impossible to make a package.")
if(!require("formatR")) stop("impossible to make a package.")
if(!require("devtools")) stop("impossible to make a package.")

####SCRIPT DIRECTORY
dirscript <- "C:/Users/AD244905/Documents/dev/biodb"
####DESTINATION DIRECTORY
pdir <- "C:/Users/AD244905/Documents/biodb"

####TEMPORARY BUFFER FILES
tempname <- "temp"

short_description <- "Connection and parsing of metabolomics databases."
long_description <- "Connection and parsing of metabolomics databases"
authors <- "Pierrick Roger-Mele"
version <- "0.1.0"
license <- "???"

####PDIR DOIT AVOIR ETE CREE.
# dirscript est le repetoire ou les scripts sont stockés.
# pdir est un repertoire qui porte le nom du furtur package. Il sera vidé.
# tempname est le nom du dossier ou seront stocke les fichiers temporaires (ils sont supprimes après le traitement)
# cleaning dit si la fonction tidy_dir doit etre utilise
# makeEmpty dit si pdir doit être nettoye
makePackageSkel<-function(dirscript,pdir,tempname = "temp", cleaning = TRUE, makeEmpty = TRUE){
	if(!dir.exists(dirscript)) stop(paste0("the given repository ",dirscript," is not  a directory"))
	if(!dir.exists(pdir)) stop(paste0("you need to create the rpository to make the dir"))
	
	if(makeEmpty){
		todelete <- list.files(pdir,recursive = TRUE,include.dirs = TRUE,full.names = TRUE)
		unlink(todelete,recursive = TRUE)
	}
	
	
	lf<-list.files(dirscript,pattern = "\\.R$",full.names = TRUE)
	
	package_dependency <- character(0)
	ndep <- 0
	###Creating the temporary directory
	tpath <- file.path(getwd(),tempname)
	if(!dir.exists(tpath)){
	    dir.create(tpath)
	}
	if(length(lf)==0) stop("only rfiles supported")
	
	###Creating the dependency graph.
	#print(basename(lf))
	
	depgraph <- make_graph(character(0),isolates = basename(lf),directed=TRUE)
	dllfiles <- character(0)
	ndll <- 0
	npa <- basename(pdir)
	for(i in 1:length(lf)){
		toremove <- numeric(0)
		
		nfile <- basename(lf[i])
		cat("\nProcessing : ",nfile,"\n")
		con <- file(lf[i],open = "r")
		lines <- readLines(con)
		print(grep(pattern = "^if",lines[1]))
		if(grepl(pattern = "^if",lines[1])){
			toremove <- c(toremove,1,length(lines))
		}
		
		###Getting all the problematic lines
		regexsource <- "\\w*source*\\('(.+)'\\)\\w*"
		regexdll <- "\\w*dyn\\.load\\('(.+)'\\)\\w*"
		regexrequire <-  "\\w*require\\w*\\((\\w*)\\)\\w*"
		regexlibrary <-  "\\w*library\\w*\\((\\w*)\\)\\w*"
		
		treg <- regexpr(regexsource,lines,perl=TRUE)
		tdll <- regexpr(regexdll,lines,perl=TRUE)
		treq <- regexpr(regexrequire,lines,perl=TRUE)
		tlib <- regexpr(regexlibrary,lines,perl=TRUE)
		pmatch <- which(treg != -1)
		
		
		
		if(length(pmatch) != 0){
		
		dfdep <- data.frame(strm = lines[pmatch],clength = attr(treg,"capture.length")[pmatch],
							cstart = attr(treg,"capture.start")[pmatch],stringsAsFactors = FALSE)
		vfiles <- apply(dfdep,1,function(x){
			
			substr(x[1],as.numeric(x[3]),as.numeric(x[3])+as.numeric(x[2])-1)
		})
		toremove <- c(toremove,pmatch)
		
		toadd <- character(length(vfiles)*2)
		for(i in 1:length(vfiles)){
			toadd[c(2*(i-1)+1,2*i)]=c(vfiles[i],nfile)
		}
		depgraph <- add.edges(depgraph, edges = toadd)
		
		
		}
		pmatch <- which(tdll != -1)
		if(length(pmatch) != 0){
			
			
			dfdep <- data.frame(strm = lines[pmatch],clength = attr(tdll,"capture.length")[pmatch],
								cstart = attr(tdll,"capture.start")[pmatch],stringsAsFactors = FALSE)
			
			ondll <- ndll+1
			ndll <- ndll+length(pmatch)
			tempdll <- apply(dfdep,1,function(x){
				
				substr(x[1],as.numeric(x[3]),as.numeric(x[3])+as.numeric(x[2])-1)
			})
			tempdll <- file.path(rep(dirname(lf[i]),length(pmatch)),tempdll)
			
			dllfiles[ondll:ndll] <- tempdll
			toremove <- c(toremove,pmatch)
		}
		
		pmatch <- which(treq != -1|tlib != -1)
		if(length(pmatch) != 0){
			
			dfdep <- data.frame(strm = lines[pmatch],
								clength = mapply(max,attr(treq,"capture.length")[pmatch],
												 attr(tlib,"capture.length")[pmatch]),
								cstart = mapply(max,attr(treq,"capture.start")[pmatch],
												attr(tlib,"capture.start")[pmatch]),
								stringsAsFactors = FALSE)
			
			ondep <- ndep+1
			ndep <- ndep+length(pmatch)
			package_dependency[ondep:ndep] <- apply(dfdep,1,function(x){
				
				substr(x[1],as.numeric(x[3]),as.numeric(x[3])+as.numeric(x[2])-1)
			})
			
			###Removing problematic lines.
			toremove <- c(toremove,pmatch)
		}
		
		close(con)
		
		####Creating a modified version of the file.
		nlines <- lines
		if(length(toremove)>0)  nlines <- nlines[-toremove]
		
		###Openning the new file,
		nfname <- file.path(tpath,nfile)
		outcon <- file(nfname)
		writeLines(nlines, outcon)
		close(outcon)
	}
	#coords <- layout_(depgraph,as_star())
	plot(depgraph)
	
	
	
	package_dependency = unique(package_dependency)
	
	cat("Package Dependency : ",package_dependency,"\n")
	cat("DLL found : ",dllfiles,"\n")
	
	###Getting the order of files accession :
	
	orderloading <- topo_sort(depgraph,mode="out")
	
	cat("File order Made :",basename(lf[orderloading]),"\n")
	nfiles <- file.path(pdir,"R", paste0(npa,"-package.R"))
	print(nfiles)
	
	
	lf=c(lf,nfiles)
	orderloading=c(as.numeric(orderloading),length(lf))
	
	strload <- character(length(orderloading)) 
	for(i in 1:length(orderloading)){
		strload [i] <- paste0("    '",basename(lf[orderloading[i]]),"'\n")
	}
	strload <- paste(strload,collapse = "")
	print(strload)
	###Creating the package directory
	
	options <- list("Imports"=package_dependency,"NeedsCompilation"="yes",
					"Collate"=strload,"Title" = short_description, "Description" = long_description,
					"Author"=authors,"Version"=version,"License"=license)
	
	
	if(length(list.files(pdir))==0){
		
		create(pdir,rstudio= FALSE,description = options)
	}
	if(!dir.exists(file.path(pdir,"R"))){
		stop("not an empty directory or a package directory")
	}
	vlines <- paste0("#' @useDynLib ",npa,"\n",'NULL')
	writeLines(vlines,con = nfiles)
	
	###Copying each directory in the pdir directory
	lmodif <- list.files(tpath,full.names = TRUE)
	file.copy(from=lmodif,to=file.path(pdir,"R"))
	
	
	
	if(length(dllfiles)>0){
	if(!dir.exists(file.path(pdir,"src"))){
		dir.create(file.path(pdir,"src"))
	}

	###Trying to copy the dll file to the src.
	rawsrc <- gsub(dllfiles,pattern = "\\.dll",replacement = "\\.c")
	existingfiles <- sapply(rawsrc,file.exists)
	print(rawsrc)
	if(!all(existingfiles)){
		stop("Missing src files")
	}
	for(i in 1:length(rawsrc))
		dest <- file.path(pdir,"src",basename(rawsrc[i]))
	    file.copy(rawsrc[i],to = dest)
	}
	
	###Creating a R package name
	
	
	
	##Cleaning the source code.
	#tidy_dir(file.path(pdir,'R'))
	
	###Creating the documentation 
	roxygenise(pdir)
	
	###Removing temp files
	todelete <- list.files(tempname,recursive = TRUE,include.dirs = TRUE,full.names = TRUE)
	unlink(todelete,recursive = TRUE)
	
	cat("Finished.")
	
	return(list(g=depgraph,dep=package_dependency,dll=dllfiles,og=basename(lf[orderloading])))
}

res <- makePackageSkel(dirscript,pdir,tempname)

install.packages(pdir,type="source",repos=NULL)
