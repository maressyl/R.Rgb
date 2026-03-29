# Draws a density plot from a histogramm for each row of a slice

draw.histdensity = function(
		slice,
		start,
		end,
		column = "value",
		pointColor = "#666666",
                lineColor = "#00007A",
                fillColor = "#4540CF",
		cex.lab = 1,
		cex = 0.2,
		pch = "+",
		bty = "o",
		fg = "#000000",
                lwd = 0.5,
                span = 0.01,
                ...
		
	) {
	# Coercions
	if(is.numeric(start)) start <- as.integer(start)
	if(is.numeric(end))   end <- as.integer(end)
	
	# Checks
	if(!is.integer(start))         stop("'start' must be integer or numeric")
	if(!is.integer(end))           stop("'end' must be integer or numeric")
	if(!is.data.frame(slice))      stop("'slice' must be a data.frame")
	if(!"start" %in% names(slice)) stop("'slice' needs a 'start' column")
	if(!"end" %in% names(slice))   stop("'slice' needs a 'end' column")
	if(!column %in% names(slice))  stop("'column' can not be found in 'slice'")
	
	# Background
	draw.bg(
		start = start,
		end = end,
		cex.lab = cex.lab,
		bty = bty,
		fg = fg,
		...
	)
	
	if(nrow(slice) > 0) {
		# Point color
		if(is.function(pointColor)) {
			environment(pointColor) <- environment()
			pointColor <- pointColor()
		}

                # smooth function
                 x = (slice$start + slice$end)/2
                 y = slice[[column]]
                 lo <- loess(y~x, span = span)
            
		# Points
	    	 graphics::points(
			x = (slice$start + slice$end)/2,
			y = slice[[column]],
			pch = pch,
			col = 'white',
			cex = cex
                        )
                # Add loess
                 xl = seq(min(x),max(x), (max(x) - min(x))/1000)
                 yl = predict(lo,xl)

                 # Fill loess
                 polygon(c(min(xl), xl, max(xl)), c(min(yl), yl, min(yl)), col=fillColor)

                 # Draw line
                 graphics::lines(
                          x = xl,
                          y = yl,
                          col=lineColor,
                          lwd=lwd)

            
	} else {
		# No box (not enough)
		graphics::text(
			x = mean(graphics::par("usr")[1:2]),
			y = mean(graphics::par("usr")[3:4]),
			label = paste(nrow(slice), "element(s) in this range"),
			col = fg,
			adj = c(0.5, 0.5),
			cex = cex.lab
		)
	}
	
	# Surrounding box
	graphics::box(
		which = "plot",
		col = fg,
		bty = bty
	)
}

