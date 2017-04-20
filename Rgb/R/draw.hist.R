# Draws an histogram with a bar for each row of slice
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

draw.hist = function(
		slice,
		start,
		end,
		column = "value",
		colorVal = "#666666",
		colorFun = function() NULL,
		border = "#666666",
		cex.lab = 1,
		origin = 0,
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
		# Draw high boxes behind
		slice <- slice[ order(slice[[column]], decreasing=TRUE) ,]
		
		# Color function
		if(is.na(colorVal)) {
			environment(colorFun) <- environment()
			boxColor <- colorFun()
		} else {
			boxColor <- colorVal
		}
		
		# Repercute to border
		if(identical(border, "color")) border <- boxColor
		
		# Boxes
		graphics::rect(
			xleft = slice$start,
			xright = slice$end,
			ytop = slice[[column]],
			ybottom = if(is.numeric(origin)) { origin } else { slice[[origin]] },
			col = boxColor,
			border = border
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
