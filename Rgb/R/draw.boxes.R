# Draws boxes for each row of slice
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

draw.boxes = function(
		slice,
		start,
		end,
		maxElements = 50,
		maxDepth = 100,
		label = TRUE,
		labelStrand = FALSE,
		labelCex = 1,
		labelSrt = 0,
		labelAdj = "center",
		labelOverflow = TRUE,
		labelFamily = "sans",
		colorVal = "#BBBBBB",
		colorFun = function() NULL,
		border = "#666666",
		cex.lab = 1,
		spacing = 0.2,
		bty = "o",
		groupBy = NA,
		groupPosition = NA,
		groupSize = NA,
		groupLwd = 1,
		...
	) {
	# Coercions
	if(is.numeric(start)) start <- as.integer(start)
	if(is.numeric(end))   end <- as.integer(end)
	
	# Checks
	if(!is.integer(start))                                                  stop("'start' must be integer or numeric")
	if(!is.integer(end))                                                    stop("'end' must be integer or numeric")
	if(!is.data.frame(slice))                                               stop("'slice' must be a data.frame")
	if(isTRUE(label) && !"name" %in% names(slice))                          stop("'slice' needs a 'name' column when 'label'")
	if(isTRUE(label) && isTRUE(labelStrand) && !"strand" %in% names(slice)) stop("'slice' needs a 'strand' column when 'label' and 'labelStrand'")
	if(!"start" %in% names(slice))                                          stop("'slice' needs a 'start' column")
	if(!"end" %in% names(slice))                                            stop("'slice' needs a 'end' column")
	
	# Background
	draw.bg(
		start = start,
		end = end,
		cex.lab = cex.lab,
		bty = bty,
		...	
	)
	
	errorMessage <- NA
	if(nrow(slice) == 0) {
		# No element
		errorMessage <- "No feature in the region"
	} else if(nrow(slice) > maxElements) {
		# Too much element
		errorMessage <- sprintf("'maxElements' reached (%i)", nrow(slice))
	} else {
		
		## COLLISION BOXES ##
		
		# Define collision boxes
		if(is.na(groupBy)) {
			# No grouping factor (all rows are individual boxes)
			boxes <- data.frame(
				start.plot = as.integer(slice$start),
				end.plot = as.integer(slice$end),
				strand = as.character(slice$strand),
				label = slice$name,
				stringsAsFactors = FALSE
			)
		} else if(!groupBy %in% names(slice)) {
			# Invalid grouping factor
			stop("'groupBy' must refer to an existing column")
		} else if(!is.na(groupPosition) && ! groupPosition %in% names(slice)) {
			# Invalid grouping factor
			stop("'groupPosition' must be NA or refer to an existing column")
		} else if(!is.na(groupSize) && ! groupSize %in% names(slice)) {
			# Invalid grouping factor
			stop("'groupSize' must be NA or refer to an existing column")
		} else {
			# Pre-process grouping factor
			if(is.factor(slice[[ groupBy ]])) {
				# Forget unused levels (10x faster via as.character)
				slice[[ groupBy ]] <- factor(as.character(slice[[ groupBy ]]))
			} else {
				# Turn other type into factors (lapply will do anyway)
				slice[[ groupBy ]] <- factor(slice[[ groupBy ]])
			}
			
			# Group rows in boxes
			boxes <- data.frame(
				start.plot = as.integer(lapply(X=split(x=slice$start,                  f=slice[[ groupBy ]]), FUN=min, na.rm=TRUE)),
				end.plot   = as.integer(lapply(X=split(x=slice$end,                    f=slice[[ groupBy ]]), FUN=max, na.rm=TRUE)),
				strand     = as.character(lapply(X=split(x=as.character(slice$strand), f=slice[[ groupBy ]]), FUN="[", i=1L)),
				label      = levels(slice[[ groupBy ]]),
				stringsAsFactors = FALSE
			)
			
			# Enlarge boxes to out-of-range features
			if(!is.na(groupPosition) && !is.na(groupSize)) {
				# Boundaries of groups observed
				start.i <- as.integer(lapply(X=split(x=slice[[ groupPosition ]], f=slice[[ groupBy ]]), FUN=min, na.rm=TRUE))
				end.i   <- as.integer(lapply(X=split(x=slice[[ groupPosition ]], f=slice[[ groupBy ]]), FUN=max, na.rm=TRUE))
				size.i  <- as.integer(lapply(X=split(x=slice[[ groupSize ]],     f=slice[[ groupBy ]]), FUN="[", i=1L))
				
				# Enlarge partially displayed boxes to drawing boundary
				boxes[ boxes$strand == "+" & start.i > 1L , "start.plot" ] <- as.integer(graphics::par("usr")[1])
				boxes[ boxes$strand == "-" & start.i > 1L , "end.plot" ] <- as.integer(graphics::par("usr")[2])
				boxes[ boxes$strand == "+" & end.i < size.i , "end.plot" ] <- as.integer(graphics::par("usr")[2])
				boxes[ boxes$strand == "-" & end.i < size.i , "start.plot" ] <- as.integer(graphics::par("usr")[1])
			}
		}
		
		# Compute collision
		boxes <- yline(
			boxes = boxes,
			start = start,
			end = end,
			label = label,
			labelStrand = labelStrand,
			labelCex = labelCex,
			labelSrt = labelSrt,
			labelAdj = labelAdj,
			labelOverflow = labelOverflow,
			maxDepth = maxDepth
		)
		
		# Break if an error occured
		if(is(boxes, "error")) {
			# Pass error
			errorMessage <- conditionMessage(boxes)
		} else {
			# From box line to (feature's) plot line
			if(is.na(groupBy)) {
				# boxes = slice
				slice$plotLine <- boxes$yline
			} else {
				# Retrieve corresponding box
				slice$plotLine <- boxes$yline[ match(slice[[ groupBy ]], boxes$label) ]
			}
			
			# Maximal depth used
			maxLine <- max(boxes$yline) + 1L
			
			
			
			## FEATURE PLOTING ##
			
			# Color function
			if(is.na(colorVal)) {
				environment(colorFun) <- environment()
				color <- colorFun()
			} else {
				color <- colorVal
			}
			
			# Repercute to border
			if(identical(border, "color")) border <- color
			
			# Group bonds
			if(!is.na(groupBy)) {
				graphics::segments(
					x0 = boxes$start.plot,
					y0 = (boxes$yline + 0.5) / maxLine,
					x1 = boxes$end.plot,
					y1 = (boxes$yline + 0.5) / maxLine,
					col = border,
					lwd = groupLwd
				)
			}
			
			# Individual boxes (limit to plotting range to work around R plot bug)
			slice$start <- pmax(graphics::par("usr")[1], slice$start)
			slice$end   <- pmin(graphics::par("usr")[2], slice$end)
			graphics::rect(
				xleft = slice$start,
				xright = slice$end,
				ytop = (slice$plotLine + (1 - spacing/2)) / maxLine,
				ybottom = (slice$plotLine + spacing/2) / maxLine,
				col = color,
				border = border
			)
			
			# Box labels
			if(isTRUE(label)) {
				# Background
				if(!is.na(groupBy)) {
					charHeight <- graphics::yinch(graphics::par("cin")[2]) * labelCex
					graphics::rect(
						xleft = boxes$start.lab,
						xright = boxes$end.lab,
						ybottom = (boxes$yline + 0.5) / maxLine - charHeight/2,
						ytop = (boxes$yline + 0.5) / maxLine + charHeight/2,
						col = "#FFFFFF",
						border = border
					)		
				}
				
				# Add strand to labels (collision already accounts for it)
				if(isTRUE(labelStrand)) {
					boxes[ boxes$strand == "-" , "label" ] <- sprintf("< %s", boxes[ boxes$strand == "-" , "label" ])
					boxes[ boxes$strand == "+" , "label" ] <- sprintf("%s >", boxes[ boxes$strand == "+" , "label" ])
				}
				
				# Plotting arguments
				args <- with(
					boxes[ (isTRUE(label) && isTRUE(labelOverflow)) | !boxes$overflow ,],
					list(
						x = (boxes$start.lab + boxes$end.lab) / 2,
						y = (boxes$yline + 0.5) / maxLine,
						label = label,
						col = "#000000",
						adj = c(0.5, 0.5),
						cex = labelCex,
						srt = labelSrt
					)
				)
				
				# Font family
				if(labelFamily == "Hershey") { args$vfont <- c("sans serif", "bold")
				} else                       { args$family <- labelFamily
				}
				
				# Execute plotting
				do.call(graphics::text, args)
			}
		}
	}
	
	# Plot only a message
	if(!is.na(errorMessage)) {
		graphics::text(
			x = mean(graphics::par("usr")[1:2]),
			y = mean(graphics::par("usr")[3:4]),
			label = errorMessage,
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

