# Non interactive genome browsing
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

browsePlot = function(
		drawables,
		chrom = NA,
		start = NA,
		end = NA,
		customLayout = FALSE,
		xaxt = "s",
		xaxm = 1.5,
		panelWidth = "5 cm",
		panel = NA,
		...
		)
	{
	# Check tracks
	if(!is(drawables, "drawable.list")) stop("'drawables' must be a 'drawable.list' object")
	drawables$check(warn=FALSE)
	
	# Break if no track
	if(drawables$count > 0) {
		# Ignore hidden objects (showing hidden=NA)
		toProcess <- which(!sapply(drawables$hidden, isTRUE))
		if(length(toProcess) > 0) {
			# Checks
			assemblies <- rep(NA_character_, drawables$count)
			organisms <- rep(NA_character_, drawables$count)
			for(i in toProcess) {
				if("assembly" %in% names(drawables$get(i)$getRefClass()$fields())) {
					assemblies[i] <- drawables$get(i)$assembly
				}
				if("organism" %in% names(drawables$get(i)$getRefClass()$fields())) {
					organisms[i] <- drawables$get(i)$organism
				}
			}
			
			# Warnings
			if(length(unique(stats::na.omit(assemblies))) > 1) warning("'drawables' contains objects from distinct assemblies")
			if(length(unique(stats::na.omit(organisms))) > 1)  warning("'drawables' contains objects from distinct organisms")
			
			# Default values
			if(is.na(chrom)) stop("'chrom' must be provided")
			if(is.na(start)) start <- 0L
			if(is.na(end))   end <- drawables$getChromEnd(chrom)
			
			# Forces integer coordinates (for 'sliceable')
			if(abs(start) > .Machine$integer.max) stop("'start' is too large (integer limit reached)")
			if(abs(end) > .Machine$integer.max)   stop("'end' is too large (integer limit reached)")
			start <- as.integer(floor(start))
			end <- as.integer(ceiling(end))
			if(end == start) end <- end - 1L
			
			# Panel display
			if(is.na(panel)) {
				# Let tracks decide
				panel <- FALSE
				for(i in toProcess) panel <- panel || drawables$get(i)$getParam("panel")
			} else {
				# Manually decided
				panel <- as.logical(panel)
			}
			
			if(!isTRUE(customLayout)) {
			
				## LAYOUT CHECK ##
				
				# Ignore new=TRUE
				toLay <- integer(0)
				for(i in toProcess) if(!drawables$objects[[i]]$getParam("new")) toLay <- c(toLay, i)
				
				# Track heights
				trackHeights <- character(0)
				for(i in toLay) trackHeights <- c(trackHeights, drawables$get(i)$getParam("height"))
				
				# Absolute tracks (from cm to inches)
				absolute = grepl("^([0-9\\.]+) cm$", trackHeights)
				heights = sum(as.double(gsub("^([0-9\\.]+) cm$", "\\1", trackHeights[absolute]))) / 2.54
				
				# Relative tracks (0.2 inches minimum)
				heights = heights + sum(!absolute) * 0.2
				
				# Check
				if(heights > graphics::par("din")[2]) stop("Plot area too small")
				
				
				
				## LAYOUT ##
				
				if(panel) {
					layout <- list(
						mat = matrix(data=1:(2 * length(toLay)), ncol=2),
						widths = c(panelWidth, 1),
						heights = trackHeights
					)
				} else {
					layout <- list(
						mat = matrix(data=1:length(toLay), ncol=1),
						heights = trackHeights
					)
				}
				
				# Get layout
				if(is.na(customLayout)) return(layout)
				
				# Apply layout
				do.call(what=graphics::layout, args=layout)
				on.exit(graphics::layout(1), add=FALSE)
				
			}
			
			
			## TRACKS ##
			
			# X axis for last track
			if(xaxt != "n") {
				# Focus on last track
				lastTrack <- toProcess[ length(toProcess) ]
				
				# Get last track's 'mar'
				mar <- drawables$get(lastTrack)$getParam("mar")
				arguments <- list(...)
				if("mar" %in% names(arguments)) mar <- arguments$mar
			
				# Update lower margin
				mar[1] <- max(mar[1], xaxm)
			}
			
			# Panels
			if(panel) {
				for(i in toProcess) {
					# new=TRUE must apply to both tracks and panels
					if(isTRUE(drawables$get(i)$getParam("new"))) graphics::par(new=new)
					
					if(drawables$get(i)$getParam("panel")) {
						if(xaxt != "n" && i == lastTrack) { drawables$get(i)$drawPanel(chrom=chrom, start=start, end=end, xaxt=xaxt, mar=mar, ...)
						} else                            { drawables$get(i)$drawPanel(chrom=chrom, start=start, end=end, xaxt="n", ...)
						}
					} else {
						# No panel, draw an empty plot
						graphics::plot(x=NA, y=NA, xlim=0:1, ylim=0:1, xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
					}
				}
			}
			
			# Tracks
			for(i in toProcess) {
				if(xaxt != "n" && i == lastTrack) { drawables$get(i)$draw(chrom=chrom, start=start, end=end, xaxt=xaxt, mar=mar, ...)
				} else                            { drawables$get(i)$draw(chrom=chrom, start=start, end=end, xaxt="n", ...)
				}
			}
			
			# Track graphical parameters to return (last track)
			outPar <- graphics::par()
			outPar$chrom <- chrom
			outPar$panel <- panel
			
			invisible(outPar)
		}
	}
}

