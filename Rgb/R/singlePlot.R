# Plots all chromosomes on a single plot, with boxes representing the track content
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

singlePlot <- function(
		object,
		bands,
		columns = 4,
		bandHeight = lcm(1),
		exclude = c("X", "Y"),
		object.args = list(),
		bands.args = list()
	) {
	# Check
	if(!is(object, "drawable"))   stop("'object' must inherit from the 'drawable' class")
	if(!is(bands, "track.bands")) stop("'bands' must inherit from the 'track.bands' class")
	
	# Erase arms and copy objects
	bands <- bands$eraseArms(temp=TRUE)
	if(is(object, "track.table")) { object <- object$eraseArms(temp=TRUE)
	} else                        { object <- object$copy(shallow=FALSE)
	}
	
	# Layout
	chromLengths <- with(bands$extract(), tapply(end, chrom, max))
	chromLengths <- chromLengths[ ! names(chromLengths) %in% exclude ]
	
	# Layout matrix
	n <- length(chromLengths)
	if(n %% columns == 0) { lay <- n
	} else                { lay <- n + (columns - (n %% columns))
	}
	lay <- matrix(1:(lay*2), ncol=columns)
	
	# Layout cell sizes
	widths <- split(x=rep(chromLengths, each=2), f=rep(1:ncol(lay), each=nrow(lay))[ 1 : (length(chromLengths)*2) ])
	widths <- sapply(widths, max) + 20e6
	heights <- rep(c(1, bandHeight), length.out=nrow(lay))
	
	# Apply layout
	layout(lay, widths=widths, heights=heights)
	par(oma=c(1,1,1,1))
	
	# Merge argument lists (object)
	object$setParam("mar", c(0, 0, 0, 0))
	object$setParam("bty", "n")
	object$setParam("xgrid", FALSE)
	object$setParam("xaxt", "n")
	object$setParam("yaxt", "n")
	object$setParam("yaxs", "i")
	object$setParam("ysub", "")
	object$setParam("ylab", "")
	for(argName in names(object.args)) object$setParam(argName, object.args[[argName]])
	
	# Merge argument lists (bands)
	bands$setParam("mar", c(0, 0, 0.5, 0))
	bands$setParam("bty", "n")
	bands$setParam("xgrid", FALSE)
	bands$setParam("xaxt", "n")
	bands$setParam("yaxt", "n")
	bands$setParam("yaxs", "i")
	bands$setParam("ysub", "")
	bands$setParam("ylab", "")
	bands$setParam("label", FALSE)
	bands$setParam("border", NA)
	bands$setParam("ylim", c(0.25, 0.75))
	for(argName in names(bands.args)) bands$setParam(argName, bands.args[[argName]])
	
	chromosomes <- setdiff(bands$chromosomes(), exclude)
	for(i in 1:length(chromosomes)) {
		chrom <- chromosomes[i]
		start <- 0L
		end <- widths[ col(lay)[i*2] ]
		
		# User object plotting
		object$draw(chrom=chrom, start=start, end=end)
		
		# Band track plotting
		bands$draw(chrom=chrom, start=start, end=end)
		xbox <- c(0, par("cxy")[1]*2*1) + par("cxy")[1]/2
		rect(xleft=xbox[1], xright=xbox[2], ybottom=0.35, ytop=0.65, border="#000000", col="#FFFFFF")
		text(x=sum(xbox)/2, y=0.5, labels=sprintf("%s", chrom), adj=c(0.5, 0.5), cex=1, font=2)
		rect(xleft=start, xright=chromLengths[chrom], ybottom=par("usr")[3], ytop=par("usr")[4])
	}
}

