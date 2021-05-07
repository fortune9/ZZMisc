# string processing
#' Generate random string
#'
#' This function generates random string of given length, always
#' starting with 2 letters

rand_str<-function(length=10) {
  part1<-paste(sample(c(letters), 2), collapse = "")
  part2<-paste(sample(c(letters, 0:9), length-2, replace=T), collapse = "")
  paste0(part1,part2)
}

## a temp file name
get_tmp_file<-function(tmpDir=".", prefix="",suffix="", sep="") {
    tmpFile<-paste(prefix, rand_str(), suffix, sep=sep)
    file.path(tmpDir, tmpFile)
}

get_temp_file=get_tmp_file

## wrap text strings
wrap_text<-function(..., lineLen=72, indent=0, exdent=0) {
    s<-paste(..., collapse=" ")
    s<-gsub("\\n+", " ", s)
    s<-gsub("\\s+", " ", s)
    s<-sub("^\\s+","",s)
    s<-sub("\\s+$","",s)
    s<-strwrap(s, width=lineLen, indent=indent, exdent=exdent)
    return(paste(s, collapse="\n"))
}

# I/O
## Report information
#
info<-function(...)
{
        message(sprintf("[%s] %s", Sys.time(), paste(..., collapse=" ") ))
}

warn<-function(...)
{
    info("[WARN]", ...)
}

#' Load a package
#'
#' Load a package, and install it if not installed yet
#'
#' @param pkg A string for package name
#'
#' @importFrom utils install.packages
#'
#' @export
load_package<-function(pkg)
{
	if(!requireNamespace(pkg, quietly=T))
	{
		message(sprintf("Package %s doesn't exist; installing it now", pkg))
		install_package(pkg)
	}
	#message(sprintf("Package %s is successfully loaded", pkg))
    require(pkg, character.only=T)
}

cran_install<-function(pkg) {
		suppressMessages(
			install.packages(
				pkg,
				dependencies=T,
				quiet=T,
				repos="https://cloud.r-project.org"
				)
			)
		# return True if sucess, False if failed
	    requireNamespace(pkg, quietly=T)
}

bioc_install<-function(pkg) {
	if (!requireNamespace("BiocManager", quietly = TRUE)) {
    	suppressMessages(
			install.packages("BiocManager",
				quiet=T,
				repos="https://cloud.r-project.org"
				)
			)
	}
	suppressMessages(BiocManager::install(pkg, update=F))
	# return True if sucess, False if failed
	requireNamespace(pkg, quietly=T)
}

#' Install packages from CRAN or Bioconductor
#'
#' A convenient function to install R packages from CRAN or
#' Bioconductor. It tries both repositories and throws failure if
#' failed in both cases.
#' 
#' @param pkg A length-one character vector containing package name
#' @export
install_package<-function(pkg)
{
	if(cran_install(pkg)) {
		return(T)
	} else if(bioc_install(pkg)) {
		return(T)
	} else {
		stop(sprintf("Package %s can't be installed from CRAN and BioConductor",
			pkg)
		)
	}
}


