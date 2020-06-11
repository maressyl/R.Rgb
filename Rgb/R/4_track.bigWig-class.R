# Generic reference class for genomic track, containing elements genomically located (chrom:start-end)
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

# A self sufficient table-shaped track element (needs to contains 'chrom', 'start' and 'end')
setRefClass(
	Class = "track.bigWig",
	contains = "crossable",
	fields = list(
		bwPath = "character",
		bigWigToBedGraph = "character",
		organism = "character",
		assembly = "character",
		addChr = "logical"
	),
	methods = list(

check = function(warn=TRUE) {
"Raises an error if the object is not valid, else returns TRUE"
	
	# crossable
	callSuper(warn)
	
	# Fields
	if(length(bwPath) != 1L || is.na(bwPath) || !file.exists(bwPath)) stop("'bwPath' must refer to an existing file")
	if(length(organism) != 1)                                         stop("'organism' must be a single character value")
	if(length(assembly) != 1)                                         stop("'assembly' must be a single character value")
	if(length(addChr) != 1)                                           stop("'addChr' must be a single logical value")
	if(is.na(addChr))                                                 stop("'addChr' cannot be NA")
	
	# Check bigWigToBedGraph
	out <- try(suppressWarnings(system2(bigWigToBedGraph, stdout=TRUE, stderr=TRUE)), silent=TRUE)
	if(is(out, "try-error"))                stop("'bigWigToBedGraph' shell execution failed")
	if(!grepl("^bigWigToBedGraph", out[1])) stop("'bigWigToBedGraph' doesn't seem to refer to the bigWigToBedGraph tool (unexpected output)")
	
	# Warnings
	if(isTRUE(warn)) {
		if(is.na(organism)) warning("'organism' should not be NA")
		if(is.na(assembly)) warning("'assembly' should not be NA")
	}
	
	return(TRUE)
},

defaultParams = function(...) {
"Returns class-specific defaults for graphical parameters. Inheriting class should overload it to define their own defaults.
- ...   : may be used by inheriting methods, especially for inter-dependant parameters."
	
	params <- callSuper(...)
	
	params$drawFun <- "draw.hist"
	params$border <- NA
	params$ylab <- .self$name
	params$ysub <- .self$assembly
	params$yaxt <- "y"
	params$yaxs <- "i"
	params$ylim <- c(0L, NA)
	
	return(params)
},

getChromEnd = function(chrom) {
"Returns as a single integer value the ending position of the object description of the given chromosome. NA (integer) is valid if non relevant, but should be avoided when possible.
- chrom   : single integer, numeric or character value, the chromosomal location. NA is not required to be handled."
	
	# Slicing the whole chromosome works but is usually too slow, better not trying
	return(as.integer(NA))
},

initialize = function(bwPath=NA_character_, bigWigToBedGraph="bigWigToBedGraph", organism=NA_character_, assembly=NA_character_, addChr=FALSE, ...) {
	callSuper(...)
	initFields(bwPath=bwPath, bigWigToBedGraph=bigWigToBedGraph, organism=organism, assembly=assembly, addChr=addChr)
},

show = function(include=FALSE, fieldWidth=10) {
"Interactive printing
- include   : single logical value, if TRUE class name will not be printed."
	
	# Class name
	if(!isTRUE(include)) {
		cat("\n  \"track.bigWig\" reference class object\n")
	} else {
		cat("\n  Extends \"track.bigWig\"\n")
	}
	
	# Fields
	cat(sprintf("  %-*s : %s\n", fieldWidth, "bwPath", bwPath[1]))
	cat(sprintf("  %-*s : %s\n", fieldWidth, "organism", organism[1]))
	cat(sprintf("  %-*s : %s\n", fieldWidth, "assembly", assembly[1]))
	
	# Inherited show()
	callSuper(include=TRUE, fieldWidth=fieldWidth)
},

size = function(chrom, start, end) {
"Count elements in the specified window.
- chrom   : single integer, numeric or character value, the chromosomal location.
- start   : single integer or numeric value, inferior boundary of the window.
- end     : single integer or numeric value, superior boundary of the window."
	
	return(nrow(.self$slice(chrom=chrom, start=start, end=end)))
},

slice = function(chrom, start, end) {
"Extract elements in the specified window as a data.frame.
- chrom      : single integer, numeric or character value, the chromosomal location.
- start      : single integer or numeric value, inferior boundary of the window.
- end        : single integer or numeric value, superior boundary of the window."
	
	if(isTRUE(addChr)) chrom <- sprintf("chr%s", chrom)
	
	if(is.numeric(chrom)) chrom <- as.integer(chrom)
	if(is.numeric(start)) start <- as.integer(start)
	if(is.numeric(end))   end <- as.integer(end)
	
	# Extract results using bigWigToBedGraph
	results <- read.table(
		pipe(
			sprintf(
				"\"%s\" \"%s\" -chrom=%s -start=%i -end=%i stdout",
				bigWigToBedGraph,
				bwPath,
				chrom,
				start,
				end
			)
		),
		sep="\t", header=FALSE, quote=NULL, comment.char="",
		col.names=c("chrom", "start", "end", "value"),
		colClasses=c("character", "integer", "integer", "numeric")
	)
	results$strand <- factor(rep(NA, nrow(results)), levels=c("-","+"))
	results$name <- rep("", nrow(results))
	
	if(isTRUE(addChr)) results$chrom <- sub("^chr", "", results$chrom)
	
	return(results)
}

	)
)

# Constructor
track.bigWig <- function(..., warn=TRUE) {
	# Inheritance
	object <- new("track.bigWig", ...)
	
	# Check
	object$check(warn=warn)
	
	return(object)
}

