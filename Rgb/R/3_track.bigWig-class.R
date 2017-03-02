# Generic reference class for genomic track, containing elements genomically located (chrom:start-end)
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

# A self sufficient table-shaped track element (needs to contains 'chrom', 'start' and 'end')
setRefClass(
	Class = "track.bigWig",
	contains = "sliceable",
	fields = list(
		file = "character",
		organism = "character",
		assembly = "character"
	),
	methods = list(

check = function(warn=TRUE) {
"Raises an error if the object is not valid, else returns TRUE"
	
	# drawable
	callSuper(warn)
	
	# Fields
	if(!file.exists(binary))  stop("'binary' file does not exist")
	if(!file.exists(file))    stop("'file' file does not exist")
	if(length(organism) != 1) stop("'organism' must be a single character value")
	if(length(assembly) != 1) stop("'assembly' must be a single character value")
	
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
	params$yaxt <- "s"
	
	params$ylab <- .self$name
	params$ysub <- .self$assembly
	
	return(params)
},

initialize = function(binary=NA_character_, file=NA_character_, organism=NA_character_, assembly=NA_character_, ...) {
	callSuper(...)
	initFields(binary=binary, file=file, organism=organism, assembly=assembly)
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
	cat(sprintf("  %-*s : %s\n", fieldWidth, "file", file[1]))
	cat(sprintf("  %-*s : %s\n", fieldWidth, "binary", binary[1]))
	cat(sprintf("  %-*s : %s\n", fieldWidth, "organism", organism[1]))
	cat(sprintf("  %-*s : %s\n", fieldWidth, "assembly", assembly[1]))
},

slice = function(chrom, start, end, asObject=FALSE) {
"Extract elements in the specified window as a data.frame.
- chrom      : single integer, numeric or character value, the chromosomal location.
- start      : single integer or numeric value, inferior boundary of the window.
- end        : single integer or numeric value, superior boundary of the window.
- asObject   : if TRUE results will be served in the same class as the current object."
	
	if(is.numeric(chrom)) chrom <- as.integer(chrom)
	if(is.numeric(start)) start <- as.integer(start)
	if(is.numeric(end))   end <- as.integer(end)
	
	# Relies on UCSC tool to slice
	out <- system2(
		command = binary,
		args = c(
			sprintf("-chrom=chr%s", chrom),
			sprintf("-start=%i", start),
			sprintf("-end=%i", end),
			file,
			"stdout"
		),
		stdout = TRUE
	)
	
	# Convert into data.frame
	tab <- read.table(
		textConnection(out),
		sep="\t", dec=".", header=FALSE, comment.char="", quote=NULL,
		col.names=c("chrom", "start", "end", "value"),
		colClasses=c("character", "integer", "integer", "double")
	)
	
	return(tab)
}

	)
)

# Constructor
track.bigWig <- function(file, binary, .name, .organism, .assembly, .parameters, warn=TRUE) {
	# Meta data
	object <- new("track.bigWig")
	if(!missing(.organism))   object$organism <- .organism
	if(!missing(.assembly))   object$assembly <- .assembly
	if(!missing(.parameters)) object$parameters <- .parameters
	
	# Files
	object$file <- normalizePath(file)
	object$binary <- normalizePath(binary)
	
	# Default name
	if(missing(.name)) { object$name <- basename(file)
	} else             { object$name <- .name
	}
	
	# Check
	object$check(warn=warn)
	
	return(object)
}

