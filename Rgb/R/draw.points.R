# Draws a scatter plot with a point for each row of slice
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

draw.points = function(
		slice,
		start,
		end,
		column = "value",
		colorVal = "#666666",
		colorFun = function() NULL,
		cex.lab = 1,
		cex = 0.6,
		pch = "+",
		bty = "o",
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
		...	
	)
	
	if(nrow(slice) > 0) {
		# Color function
		if(is.na(colorVal)) {
			environment(colorFun) <- environment()
			pointColor <- colorFun()
		} else {
			pointColor <- colorVal
		}
		
		# Points
		graphics::points(
			x = (slice$start + slice$end)/2,
			y = slice[[column]],
			pch = pch,
			col = pointColor,
			cex = cex
		)
	} else {
		# No box (not enough)
		graphics::text(
			x = mean(graphics::par("usr")[1:2]),
			y = mean(graphics::par("usr")[3:4]),
			label = paste(nrow(slice), "element(s) in this range"),
			col = "#000000",
			adj = c(0.5, 0.5),
			cex = cex.lab
		)
	}
	
	# Surrounding box
	graphics::box(
		which = "plot",
		col = "#000000",
		bty = bty
	)
}
