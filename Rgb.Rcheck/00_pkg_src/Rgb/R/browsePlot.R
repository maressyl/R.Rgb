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
			if(length(unique(na.omit(assemblies))) > 1) warning("'drawables' contains objects from distinct assemblies")
			if(length(unique(na.omit(organisms))) > 1)  warning("'drawables' contains objects from distinct organisms")
			
			# Default values
			if(is.na(chrom)) stop("'chrom' must be provided")
			if(is.na(start)) start <- 0L
			if(is.na(end)) {
				ends <- integer(0)
				for(i in toProcess) ends[i] <- drawables$get(i)$getChromEnd(chrom)
				if(all(is.na(ends))) stop("Unable to predict chromosome end. Set 'end' manually or use at least one drawable object whose getChromEnd() method does not return NA.")
				end <- max(ends, na.rm=TRUE)
			}
			
			# Forces integer coordinates (for 'sliceable')
			if(abs(start) > .Machine$integer.max) stop("'start' is too large (integer limit reached)")
			if(abs(end) > .Machine$integer.max)   stop("'end' is too large (integer limit reached)")
			start <- as.integer(floor(start))
			end <- as.integer(ceiling(end))
			if(end == start) {
				end <- end - 1L
			}
			
			
			if(!isTRUE(customLayout)) {
			
				## LAYOUT CHECK ##
				
				# Track heights
				trackHeights = character(0)
				for(i in toProcess) {
					trackHeights <- c(trackHeights, drawables$get(i)$getParam("height"))
				}
				
				# Absolute tracks (from cm to inches)
				absolute = grepl("^([0-9\\.]+) cm$", trackHeights)
				heights = sum(as.double(gsub("^([0-9\\.]+) cm$", "\\1", trackHeights[absolute]))) / 2.54
				
				# Relative tracks (0.2 inches minimum)
				heights = heights + sum(!absolute) * 0.2
				
				# Check
				if(heights > par("din")[2]) stop("Plot area too small")
				
				
				## LAYOUT ##
				
				layout(
					matrix(
						data = 1:length(toProcess),
						ncol = 1
					),
					heights = trackHeights
				)
				
				on.exit(layout(1), add=FALSE)
				
			}
			
			
			## TRACKS ##
			
			for(i in toProcess) {
				
				if(i == tail(toProcess, 1)) {
					# X axis for last track
					if(xaxt != "n") {
						# Get 'mar'
						mar <- drawables$get(i)$getParam("mar")
						arguments <- list(...)
						if("mar" %in% names(arguments)) mar <- arguments$mar
						
						# Update lower margin
						mar[1] <- max(mar[1], xaxm)
						
						# Plot track
						drawables$get(i)$draw(chrom=chrom, start=start, end=end, xaxt=xaxt, mar=mar, ...)
					} else {
						# Plot track
						drawables$get(i)$draw(chrom=chrom, start=start, end=end, xaxt="n", ...)
					}
					
					# Track graphical parameters to return
					outPar <- par()
					outPar$chrom <- chrom
				} else {
					# Plot track
					drawables$get(i)$draw(chrom=chrom, start=start, end=end, xaxt="n", ...)
				}
			}
			
			invisible(outPar)
		}
	}
}
