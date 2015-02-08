# Plots all chromosomes on a single plot, with boxes representing the track content
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

singlePlot <- function(
		track,
		bandTrack,
		columns = 4,
		exclude = c("X", "Y")
	) {
	# Erase arms
	bands <- bandTrack$eraseArms(temp=TRUE)
	elements <- track$eraseArms(temp=TRUE)
	
	# Add colors
	colors <- bands$extract(,"stain")
	colors[ colors == "gneg" ]    <- "#EEEEEE"
	colors[ colors == "gpos25" ]  <- "#CCCCCC"
	colors[ colors == "gpos50" ]  <- "#AAAAAA"
	colors[ colors == "gpos75" ]  <- "#888888"
	colors[ colors == "gpos100" ] <- "#666666"
	colors[ colors == "acen" ]    <- "#CC8888"
	colors[ colors == "gvar" ]    <- "#88CC88"
	colors[ colors == "stalk" ]   <- "#8888CC"
	bands$addColumn(colors, "colors")
	
	# Layout
	chromLengths <- with(bands$extract(), tapply(end, chrom, max))
	chromLengths <- chromLengths[ ! names(chromLengths) %in% exclude ]
	
	# Layout matrix
	n <- length(chromLengths)
	if(n %% columns == 0) { lay <- 1:n
	} else                { lay <- 1:(n + (columns - (n %% columns)))
	}
	lay <- matrix(lay, ncol=columns)
	
	# Layout widths
	chromLengths <- c(chromLengths, rep(0L, ncol(lay)*nrow(lay)-length(chromLengths)))
	widths <- tapply(chromLengths, as.vector(col(lay)), max) + 20e6
	
	# Apply layout
	layout(lay, widths=widths)
	par(oma=c(1,1,1,1))
	
	chromosomes <- setdiff(bands$chromosomes(), exclude)
	for(i in 1:length(chromosomes)) {
		# Plotted chromosome
		chrom <- bands$chromosomes()[i]
		
		# Background
		par(mar=c(0,0,0,0))
		plot(x=NA, y=NA, xlim=c(0, widths[col(lay)[i]]), ylim=c(-1,1), xaxs="i", yaxs="i", xaxt="n", yaxt="n", bty="n")
		
		# Bands
		with(bands$slice(chrom, 0, chromLengths[chrom]), rect(xleft=start, xright=end, ybottom=-0.5, ytop=-0.1, col=colors, border=NA))
		rect(xleft=0, xright=chromLengths[chrom], ybottom=-0.5, ytop=-0.1)
		
		# Track elements
		rect(xleft=0, xright=chromLengths[chrom], ybottom=0.1, ytop=0.8, col="#EEEEEE", border="#EEEEEE")
		tmp <- elements$slice(chrom, 0, chromLengths[chrom])
		if(!"colors" %in% colnames(tmp)) tmp$colors <- "#000000"
		if(!"height" %in% colnames(tmp)) tmp$height <- 1
		if(nrow(tmp) > 0) with(tmp, rect(xleft=start, xright=end, ybottom=0.1, ytop=height*0.7+0.1, col=colors, border=colors))
		
		# Chromosome name
		text(x=0, y=0.7, labels=sprintf(" %s", chrom), adj=c(0, 1), cex=1.25, font=2)
	}
}

