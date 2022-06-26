# string processing
#' Generate random string
#'
#' This function generates random string of given length, always
#' starting with 2 letters
#'
#' @export

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

#' Wrap text into paragraphs
#'
#' This program concatenates input strings and collapses consecutive
#' blanks, and output a single string with inserted newlines.
#'
#'
#' @export
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

#' Print info message
#'
#' @export
info<-function(...)
{
        message(sprintf("[%s] %s", Sys.time(), paste(..., collapse=" ") ))
}

#' Print warning message
#'
#' @export
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
				dependencies=NA,
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

#' Join dataframes
#'
#' Given a list of [data.frame] or [data.table],
#' it outputs a [data.table] by merging them together
#'
#' @param dfList A list of data.frame or data.table
#' @param setCol Whether to add a 'set' column for each input
#'  data as an indicator of data origin. When TRUE, a new 
#'  column (set1, set2, etc) is added to each input data;
#'  when FALSE, nothing is added. Alternatively, one can
#'  provide a character vector to be used as the names of
#'  the indicator columns. The elements of the indicator columns
#'  are all '1's.
#' @param setNAtoZero If TRUE, the NAs in the indicator columns
#'  are set to '0'.
#' @param ... The parameters passed to [data.table::merge()]
#'  directly
#' @return A [data.table-class] object
#'
join_df<-function(dfList, setCol=TRUE, setNAtoZero=T,...) {
    if(length(dfList) < 2) {
        return(dfList[[1]])
    }
    if(identical(setCol, T)) {
        origColNames<-paste0("Set", seq_len(length(dfList)))
    } else if(!identical(setCol, F)) {
        origColNames<-setCol
        stopifnot(length(origColNames) == length(dfList))
    } else {
        origColNames<-NULL
    }
    # start merging
    res<-data.table::data.table()
    for(i in seq_len(length(dfList)) ) {
        toMerge<-dfList[[i]]
        if(!data.table::is.data.table(toMerge)) {
            data.table::setDT(toMerge)
        }
        if(!is.null(origColNames)) {
            toMerge[[origColNames[i]]]<-1L
        }
        if(i==1) {
            res<-toMerge
        } else {
            res<-merge(res,toMerge,...)
        }
    }
    # set NAs in origin columns into zeros
    if(!is.null(origColNames) && setNAtoZero) {
        res[, (origColNames):=lapply(.SD, function(x) { x[is.na(x)]<-0L; return(x) } ), .SDcols=origColNames]
    }
    return(invisible(res))
}

#' Region overlap
#'
#' Given two lists of genomic regions, a [data.table-class] containing
#' the overlapped regions are returned.
#'
#' The input genomic regions should contain 3 columns, 'chr', 'start',
#' and 'end', and all other columns are copied to the output.
#'
#' @param x The first input genomic regions. A [data.frame()] or
#'   [data.table()].
#' @param y The second input genomic regions. A [data.frame()] or
#'   [data.table()].
#' @param regionCols1 A vector of column names or numbers in `x` giving
#'   'chr', 'start', and 'end' columns in order. Default is the first
#'   3 columns.
#' @param regionCols2 A vector of column names or numbers in `y` giving
#'   'chr', 'start', and 'end' columns in order. Default is the first
#'   3 columns.
#' @param matchedOnly A logical. If TRUE, then non-overlaped input
#'   regions won't be output. If FALSE (default), all the rows are
#'   output
#' @inheritParams data.table::foverlaps
#' @return A data.table. The columns from `x` and `y` will have
#'   suffix '.x' and '.y', respectively.
#'
region_overlap<-function(x, y, 
                         regionCols1=1:3L,
                         regionCols2=1:3L,
                         type = c("any", "within", "start", "end", "equal"),
                         mult = c("all", "first", "last"),
                         matchedOnly=F
                         ) {
    type<-match.arg(type)
    mult<-match.arg(mult)
    if(!is.data.table(x)) { setDT(x) }
    if(!is.data.table(y)) { setDT(y) }
    if(is.integer(regionCols1)) { regionCols1<-colnames(x)[regionCols1] }
    if(is.integer(regionCols2)) { regionCols2<-colnames(y)[regionCols2] }
    setkeyv(y, regionCols2) # required
    dat<-foverlaps(x[,..regionCols1],
                   y[,..regionCols2],
                   by.x=regionCols1,
                   by.y=regionCols2,
                   type=type,
                   mult=mult,
                   nomatch=NULL,
                   which=T
                   )
    if(is.vector(dat)) { # a vector is returned for 'first' and 'last' mode
        xid<-seq_len(length(dat))
        xid<-xid[dat != 0]
        yid<-dat[dat != 0]
        dat<-data.table(xid,yid)
    }
    # create result data.table by combine overlapped and nonoverlapped
    # rows
    xColnames<-paste0(colnames(x), ".x")
    yColnames<-paste0(colnames(y), ".y")
    res<-cbind(x[dat$xid,], y[dat$yid,])
    setnames(res, c(xColnames, yColnames))
    # add non-matched rows
    if(!matchedOnly) {
        if(nrow(dat) > 0) { # sometimes no overlap at all
         xSpecific<-x[-unique(dat$xid),]
         ySpecific<-y[-unique(dat$yid),]
        } else {
         xSpecific<-x
         ySpecific<-y
        }
         xSpecific[, (yColnames):=NA]
         ySpecific[, (xColnames):=NA]
         setnames(xSpecific, c(xColnames, yColnames))
         setnames(ySpecific, c(yColnames, xColnames))
         res<-rbind(res, xSpecific, ySpecific)
    }
    return(res)
}

