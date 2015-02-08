# Track class for Copy Number Variations
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

# Only defining new drawing parameters defaults
setRefClass(
	Class = "track.CNV",
	contains = "track.table",
	methods = list(
		
defaultParams = function(...) {
"Returns class-specific defaults for graphical parameters. Inheriting class should overload it to define their own defaults.
- ...   : may be used by inheriting methods, especially for inter-dependant parameters."
	
	params <- callSuper(...)
	
	params$drawFun <- "draw.boxes"
	params$maxElements <- 2000
	params$label <- FALSE
	params$labelStrand <- FALSE
	params$labelCex <- 0.75
	params$labelSrt <- 0
	params$labelAdj <- "left"
	params$colorVal <- as.character(NA)
	params$colorFun <- function() {
		output <- rep("#888888", nrow(slice))
		output[ slice$type %in% c("Duplication", "Gain", "Insertion") ] <- "#8888FF"
		output[ slice$type == "Deletion" ] <- "#FF8888"
		output[ slice$type == "Loss" ] <- "#880000"
		output[ slice$type == "Complex" ] <- "#BB7744"
		return(output)
	}
	params$border <- "color"
	params$height <- "3 cm"
	params$xaxt <- "n"
	params$yaxt <- "n"
	params$ylim <- 0:1
	params$cex.lab <- 1
	
	return(params)
},

show = function(include=FALSE, fieldWidth=10) {
"Interactive printing
- include   : single logical value, if TRUE class name will not be printed."
	
	# Class name
	if(!isTRUE(include)) { cat("\n  \"track.CNV\" reference class object\n")
	} else               { cat("\n  Extends \"track.CNV\"\n")
	}
	
	# Inherited show()
	callSuper(include=TRUE, fieldWidth=fieldWidth)
}
		
	)
)

# Constructor
track.CNV <- function(...) {
	# Inheritance
	object <- new("track.CNV")
	object$import(track.table(...))
	
	return(object)
}

