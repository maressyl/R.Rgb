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
		labelCex = 0.8,
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
				start.rect = as.integer(slice$start),
				end.rect = as.integer(slice$end),
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
				start.rect = as.integer(lapply(X=split(x=slice$start,                  f=slice[[ groupBy ]]), FUN=min, na.rm=TRUE)),
				end.rect   = as.integer(lapply(X=split(x=slice$end,                    f=slice[[ groupBy ]]), FUN=max, na.rm=TRUE)),
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
				boxes[ boxes$strand == "+" & start.i > 1L , "start.rect" ] <- as.integer(par("usr")[1])
				boxes[ boxes$strand == "-" & start.i > 1L , "end.rect" ] <- as.integer(par("usr")[2])
				boxes[ boxes$strand == "+" & end.i < size.i , "end.rect" ] <- as.integer(par("usr")[2])
				boxes[ boxes$strand == "-" & end.i < size.i , "start.rect" ] <- as.integer(par("usr")[1])
			}
		}
		
		# Label widths
		if(labelSrt == 0)         { labelWidths <- round(xinch(par("cin")[1]) * (nchar(boxes$label) + ifelse(labelStrand, 3, 1)) * labelCex) * 0.66
		} else if(labelSrt == 90) { labelWidths <- round(xinch(par("cin")[2]) * 1 * labelCex) * 0.66
		} else                    { labelWidths <- 0 #TODO
		}
		
		# Label box position
		if(labelAdj == "left") {
			# May overflow by the right
			boxes$start.lab <- as.integer(boxes$start.rect)
			boxes$end.lab <- as.integer(boxes$start.rect + labelWidths)
		} else if(labelAdj == "right") {
			# May overflow by the left
			boxes$start.lab <- as.integer(boxes$end.rect - labelWidths)
			boxes$end.lab <- as.integer(boxes$end.rect)
		} else if(labelAdj == "center") {
			# May overflow by both sides
			boxes$start.lab <- as.integer((boxes$start.rect + boxes$end.rect) / 2 - labelWidths / 2)
			boxes$end.lab <- as.integer((boxes$start.rect + boxes$end.rect) / 2 + labelWidths / 2)
		} else {
			stop("Invalid 'labelAdj'")
		}
		
		# Label out of sight (left)
		outLeft <- boxes$start.lab < start
		if(any(outLeft)) {
			boxes[ outLeft , "start.lab" ] <- as.integer(start)
			boxes[ outLeft , "end.lab" ] <- as.integer(start + labelWidths[ outLeft ])
		}
		
		# Label out of sight (right)
		outRight <- boxes$end.lab > end
		if(any(outRight)) {
			boxes[ outRight , "start.lab" ] <- as.integer(end - labelWidths[ outRight ])
			boxes[ outRight , "end.lab" ] <- as.integer(end)
		}
		
		# Final collision box boundaries
		boxes$start <- boxes$start.rect
		boxes$end <- boxes$end.rect
		boxes$overflow <- boxes$start.lab < boxes$start.rect | boxes$end.lab > boxes$end.rect
		if(isTRUE(label) && isTRUE(labelOverflow)) {
			# Allow overflow
			boxes[ boxes$overflow , "start" ] <- boxes[ boxes$overflow , "start.lab" ]
			boxes[ boxes$overflow , "end" ] <- boxes[ boxes$overflow , "end.lab" ]
		}
		
		# 'boxes' must be ordered to use subtrack
		neworder <- order(boxes$start)
		boxes <- boxes[ neworder ,]
		
		# To bypass subtrack() indexing during next step, as working on a single chromosome subtrack
		boxes$chrom <- 1L
		boxIndex <- length(boxes$start)
		
		# Looking for non overlaping boxLines (attribution from the widest box to the narrowest)
		boxLines <- rep(-1L, nrow(boxes))
		for(l in order(boxes$end - boxes$start, decreasing=TRUE)) {
			overlaps <- subtrack(1L, boxes$start[l]+1L, boxes$end[l]-1L, boxIndex, boxes, boxLines=boxLines)
			i <- 0L; while(any(overlaps$boxLines == i)) i <- i + 1L
			if(i > maxDepth) {
				errorMessage <- "'maxDepth' reached"
				break
			}
			boxLines[l] <- i
		}
		
		# Break if maxDepth has been reached
		if(is.na(errorMessage)) {
			# From box line to (feature's) plot line
			if(is.na(groupBy)) {
				# 'boxes'~='slice', but was reordered for 'boxLines' computation
				slice <- slice[ neworder ,]
				slice$plotLine <- boxLines
			} else {
				# Retrieve corresponding box
				slice$plotLine <- boxLines[ match(slice[[ groupBy ]], boxes$label) ]
			}
			
			# Maximal depth used
			maxLine <- max(boxLines) + 1L
			
			
			
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
				segments(
					x0 = boxes$start.rect,
					y0 = (boxLines + 0.5) / maxLine,
					x1 = boxes$end.rect,
					y1 = (boxLines + 0.5) / maxLine,
					col = border
				)
			}
			
			# Individual boxes (limit to plotting range to work around R plot bug)
			slice$start <- pmax(par("usr")[1], slice$start)
			slice$end   <- pmin(par("usr")[2], slice$end)
			rect(
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
					charHeight <- yinch(par("cin")[2]) * labelCex
					rect(
						xleft = boxes$start.lab,
						xright = boxes$end.lab,
						ybottom = (boxLines + 0.5) / maxLine - charHeight/2,
						ytop = (boxLines + 0.5) / maxLine + charHeight/2,
						col = "#FFFFFF",
						border = border
					)		
				}
				
				# Add strand to labels (before collision computation)
				if(isTRUE(labelStrand)) {
					boxes[ boxes$strand == "-" , "label" ] <- sprintf("< %s", boxes[ boxes$strand == "-" , "label" ])
					boxes[ boxes$strand == "+" , "label" ] <- sprintf("%s >", boxes[ boxes$strand == "+" , "label" ])
				}
				
				# Plotting arguments
				args <- with(
					boxes[ (isTRUE(label) && isTRUE(labelOverflow)) | !boxes$overflow ,],
					list(
						x = (start.lab + end.lab) / 2,
						y = (boxLines + 0.5) / maxLine,
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
				do.call(text, args)
			}
		}
	}
	
	# Plot only a message
	if(!is.na(errorMessage)) {
		text(
			x = mean(par("usr")[1:2]),
			y = mean(par("usr")[3:4]),
			label = errorMessage,
			col = "#000000",
			adj = c(0.5, 0.5),
			cex = cex.lab
		)
	}
	
	# Surrounding box
	box(
		which = "plot",
		col = "#000000",
		bty = bty
	)
}

