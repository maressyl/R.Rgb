# Draws the common background of track plots
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

draw.bg = function(
		start,
		end,
		ylab = "",
		ysub = as.character(NA),
		mar = c(0.2, 5, 0.2, 1),
		xaxt = "s",
		yaxt = "n",
		yaxs = "r",
		yaxp = as.numeric(NA),
		ylim = c(0, 1),
		cex.lab = 1,
		bty = "o",
		las = 0,
		xgrid = TRUE,
		new = FALSE,
		...
	) {
	# Coercions
	if(is.numeric(start))  start <- as.integer(start)
	if(is.numeric(end))    end <- as.integer(end)
	
	# Checks
	if(!is.integer(start)) stop("'start' must be integer or numeric")
	if(!is.integer(end))   stop("'end' must be integer or numeric")
	
	# pretty() acuraccy workaround
	if(start == -1) start <- 0L
	
	# yaxp default to NULL
	if(any(is.na(yaxp))) yaxp <- NULL
	
	# Background
	graphics::par(cex=1, mar=mar, new=new)
	graphics::plot(
		x=NA, y=NA,
		xlim = c(start, end),
		ylim = ylim,
		xlab = "",
		xaxt = "n",
		xaxs = "i",
		ylab = ylab,
		yaxt = yaxt,
		yaxs = yaxs,
		yaxp = yaxp,
		bty = "n",
		las = las,
		cex.lab = cex.lab
	)
	
	# Secondary ylab (assembly)
	if(yaxt == "n" && !is.na(ysub)) {
		graphics::mtext(
			side = 2,
			text = ysub,
			line = 1,
			adj = 0.5,
			cex = cex.lab
		)
	}
	
	# X grid and axis (Mb)
	at <- pretty(c(start, end), n=12)
	if(xaxt != "n") {
		# With axis labels
		if(isTRUE(xgrid)) { graphics::axis(side=1, at=at, las=las, tck=1, col="#CCCCCC", lty="dotted", cex.axis=cex.lab, labels=at/1e6, padj=-1)
		} else            { graphics::axis(side=1, at=at, las=las, cex.axis=cex.lab, labels=at/1e6, padj=-1)
		}
	} else {
		# Without axis labels
		if(isTRUE(xgrid)) { graphics::axis(side=1, at=at, las=las, tck=1, col="#CCCCCC", lty="dotted", cex.axis=cex.lab, labels=FALSE, padj=-1)
		}
	}
	
	# Proper box
	graphics::box(
		which = "plot",
		col = "#000000",
		bty = bty
	)
}

