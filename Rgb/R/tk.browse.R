# Interactive TCL-TK genome browser
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

tk.browse = function(
		drawables = drawable.list(),
		hscale = NA,
		vscale = NA,
		blocking = FALSE,
		scaleFactor = NA,
		updateLimit = 0.4
		)
	{
	suppressMessages(library(tcltk, quietly=TRUE))
	suppressMessages(library(tkrplot, quietly=TRUE))
	
	# Check tracks
	if(!is(drawables, "drawable.list")) stop("'drawables' must be a 'drawable.list' object")
	drawables$check(warn=FALSE)
	
	
	
	## FUNCTIONS ##
	
	checkPlot = function(silent=FALSE) {
		# No track
		if(drawables$count == 0) {
			if(!silent) {
				tkmessageBox(
					parent = topLevel,
					icon = "info",
					type = "ok",
					title = "Choose drawables",
					message = sprintf("Use the 'Tracks' button to add drawable objects to the plot.")
				)
			}
			return(FALSE)
		}
		
		# 'chrom' error
		if(as.character(tcl(chromCombo, "get")) == "") {
			if(!silent) {
				tkmessageBox(
					parent = topLevel,
					icon = "info",
					type = "ok",
					title = "Choose chromosome",
					message = sprintf("Enter a chromosome name in the 'Chromosome' field, or use 'Find'.")
				)
			}
			return(FALSE)
		}
		
		# 'start' or 'end' error
		if(!grepl("^(-?[0-9]+(\\.[0-9]+)?)?$", tclvalue(startValue)) || !grepl("^(-?[0-9]+(\\.[0-9]+)?)?$", tclvalue(endValue))) {
			if(!silent) {
				tkmessageBox(
					parent = topLevel,
					icon = "error",
					type = "ok",
					title = "Invalid coordinates",
					message = sprintf("Enter numeric coordinates, using the dot as decimal separator.")
				)
			}
			return(FALSE)
		}
		
		# No error
		return(TRUE)
	}
	
	if(is.na(scaleFactor)) {
		if(.Platform$OS.type == "unix") { scaleFactor <- 480
		} else                          { scaleFactor <- 388
		}
	}
	
	autoVScale = function() {
		return(max(1, (as.double(tclvalue(tkwinfo("height", topLevel))) - 53 - 2) / scaleFactor))
	}
	
	autoHScale = function() {
		return(max(1, (as.double(tclvalue(tkwinfo("width", topLevel))) - 10 - 2) / scaleFactor))
	}
	
	resize = function() {
		# Automatic sizes
		vscale <<- autoVScale()
		hscale <<- autoHScale()
		
		# Refresh
		replot()
	}
	
	savePar <- list()
	xConvert = function(x) {
		# Plot area coordinates in pixels
		width <- as.numeric(tclvalue(tkwinfo("reqwidth", plotWidget)))
		xMin <- savePar$plt[1] * width
		xMax <- savePar$plt[2] * width
		
		# From pixel area to x range
		return((x - xMin) / (xMax - xMin) * (savePar$usr[2] - savePar$usr[1]) + savePar$usr[1])
	}
	
	keyPressUp = function() {
		zoom("in")
	}
	
	keyPressDown = function() {
		zoom("out")
	}
	
	keyPressLeft = function() {
		move("left")
	}
	
	keyPressRight = function() {
		move("right")
	}
	
	dragFrom <- NA
	mousePress = function(x, y) {
		dragFrom <<- formatCoordinate(xConvert(as.double(x)) / 1e6)
	}
	
	dragTo <- NA
	mouseRelease = function(x, y) {
		dragTo <<- formatCoordinate(xConvert(as.double(x)) / 1e6)
		if(dragFrom != dragTo) {
			tclvalue(startValue) <- min(as.numeric(dragFrom), as.numeric(dragTo))
			tclvalue(endValue) <- max(as.numeric(dragFrom), as.numeric(dragTo))
			replot()
		}
	}
	
	mouseWheel = function(D) {
		if(D > 0) { mouseWheelUp()
		} else    { mouseWheelDown()
		}
	}
	
	mouseWheelUp = function() {
		zoom("in")
	}
	
	mouseWheelDown = function() {
		zoom("out")
	}
	
	replaceEntry = function(widget) {
		tkfocus("-force", widget)
		tcl(widget, "delete", "0", "end")
		tcl(widget, "icursor", "end")
	}
	
	replot = function() {
		if(checkPlot()) {
			# Focus
			tcl("wm", "attributes", topLevel, topmost = 1)
			
			# Default size (not at launch, for embedded compatibility)
			
			# Replot
			tkrreplot(
				lab = plotWidget,
				fun = function() {
					par(bg="#FFFFFF")
					handle(
						expr = {
							savePar <<- browsePlot(
								drawables = drawables,
								chrom = as.character(tcl(chromCombo, "get")),
								start = as.double(tclvalue(startValue)) * 1e6,
								end = as.double(tclvalue(endValue)) * 1e6
							)
						},
						# Silently ignore message()
						messageHandler = NULL,
						# Pass warning() but continue execution
						warningHandler = function(w) {
							tkmessageBox(
								parent = topLevel,
								icon = "warning",
								type = "ok",
								title = "Warning in browsePlot()",
								message = conditionMessage(w)
							)
						},
						# Pass stop() and stop execution
						errorHandler = function(e) {
							tkmessageBox(
								parent = topLevel, 
								icon = "error",
								type = "ok",
								title = "Error in browsePlot()",
								message = conditionMessage(e)
							)
						}					
					)
				},
				hscale = hscale,
				vscale = vscale
			)
			
			# Focus
			tcl("wm", "attributes", topLevel, topmost = 0)
			tkfocus(force=topLevel)
		}
	}
	
	forceCoordinates = function() {
		# Is forcing needed ?
		if(tclvalue(startValue) == "" || tclvalue(endValue) == "") {
			# Needed but impossible
			if(! "usr" %in% names(savePar) || ! "chrom" %in% names(savePar) || as.character(tcl(chromCombo, "get")) != savePar$chrom) {
				return(FALSE)
			}
			
			# Computation from previous plot
			if(tclvalue(startValue) == "") tclvalue(startValue) <- formatCoordinate(savePar$usr[1] / 1e6)
			if(tclvalue(endValue) == "")   tclvalue(endValue) <- formatCoordinate(savePar$usr[2] / 1e6)
		}
		
		return(TRUE)
	}
	
	formatCoordinate = function(x) {
		x <- as.numeric(x)
		if(abs(x) < 0.000003) x <- 0
		x <- sprintf("%.6f", round(x, 6))
		x <- sub("\\.?0+$", "", x)
		return(x)
	}
	
	updateT0 <- updateT1 <- proc.time()['elapsed']
	
	move = function(way) {
		updateT1 <<- proc.time()['elapsed']
		if(updateT1 - updateT0 > updateLimit) {
			if(checkPlot()) {
				if(forceCoordinates()) {
					# Values
					LFactor = 2
					LStart = as.double(tclvalue(startValue))
					LEnd = as.double(tclvalue(endValue))
				
					# Coordinate update
					width = (LEnd - LStart) / LFactor
					if (way == "left") { width = -width }
					tclvalue(startValue) <- formatCoordinate(LStart + width)
					tclvalue(endValue) <- formatCoordinate(LEnd + width)
					
					# Refresh timer
					updateT0 <<- updateT1
				}
			
				# Replot
				replot()
			}
		}
	}
	
	zoom = function(way) {
		updateT1 <<- proc.time()['elapsed']
		if(updateT1 - updateT0 > updateLimit) {
			if(checkPlot()) {
				if(forceCoordinates()) {
					# Values
					LFactor = 2
					LStart = as.double(tclvalue(startValue))
					LEnd = as.double(tclvalue(endValue))
				
					# Inversion
					modifier = if (way == "out") { -1 } else { LFactor }
				
					# Coordinates update
					width = (LEnd - LStart) * (LFactor-1) / modifier / 2
					tclvalue(startValue) <- formatCoordinate(LStart + width)
					tclvalue(endValue) <- formatCoordinate(LEnd - width)
					
					# Refresh timer
					updateT0 <<- updateT1
				}
				
				# Replot
				replot()
			}
		}
	}
	
	refreshChromCombo <- function() {
		# Get consensus chromosome list
		chromList <- NULL
		if(drawables$count > 0) {
			for(i in 1:drawables$count) {
				tmp <- drawables$get(i)$chromosomes()
				if(!is.null(tmp)) {
					if(is.null(chromList)) {
						# Use the first non-NULL list
						chromList <- tmp
					} else if(!setequal(chromList, tmp)) {
						# Make sure other lists are compatible
						tkmessageBox(
							parent = topLevel,
							icon = "error",
							type = "ok",
							title = "Invalid chromosome lists",
							message = sprintf("Selected objects have chromosome lists that do not totally overlap.")
						)
						chromList <- NULL
						break
					}
				}
			}
		}
		
		# Refresh the combobox
		tkconfigure(chromCombo, values=chromList)
	}
	
	refreshSearchCombo <- function() {
		values <- sub("\\.[^\\.]+$", "", drawables$names)
		tkconfigure(searchCombo, values=values)
		if(length(values) > 0) tcl(searchCombo, "set", values[1])
	}
	
	matchIterator <- 0L
	searchAction = function() {
		track <- drawables$get( as.integer(tcl(searchCombo, "current")) + 1L )
		if(tclvalue(searchValue) == "") {
			tkmessageBox(
				parent = topLevel,
				icon = "info",
				type = "ok",
				title = "Choose a target",
				message = "Enter in the next field the name of the element to search, or a regular expression."
			)
		} else if(!is(track, "track.table")) {
			tkmessageBox(
				parent = topLevel,
				icon = "error",
				type = "ok",
				title = "Unable to find the target",
				message = "The track you are searching is not a 'track.table' inheriting R object."
			)
		} else {
			# Extraction
			regexp <- as.logical(as.integer(tclvalue(searchRegexpValue)))
			if(regexp) i <- sprintf("grep(pattern=\"%s\", x=name, value=FALSE, ignore.case=TRUE)", tclvalue(searchValue))
			else       i <- sprintf("name == \"%s\"", tclvalue(searchValue))
			matches <- suppressWarnings(track$extract(parse(text=i)))
			
			if(nrow(matches) == 0) {
				# No match
				tkmessageBox(
					parent = topLevel,
					icon = "warning",
					type = "ok",
					title = "Unable to find the target",
					message = sprintf("No match for \"%s\"", tclvalue(searchValue))
				)
				return()
			} else if(nrow(matches) > 1) {
				# Increment iterator
				matchIterator <<- matchIterator + 1L
				if(matchIterator > nrow(matches)) matchIterator <<- 1L
				
				# Multiple matches
				matches <- matches[ order(matches$name) ,]
				if(nrow(matches) < 20) {
					matchList <- matches$name
					matchList[ matchIterator ] <- sprintf("current :\t%s", matchList[ matchIterator ])
					matchList[ -matchIterator ] <- sprintf("\t%s", matchList[ -matchIterator ])
					tkmessageBox(
						parent = topLevel,
						icon = "info",
						type = "ok",
						title = "Multiple matches",
						message = sprintf(
							"%d matches found for \"%s\"\nHit 'Find' to jump to the next one.\n\n%s",
							nrow(matches),
							tclvalue(searchValue),
							paste(matchList, collapse="\n")
						)
					)
				} else {
					tkmessageBox(
						parent = topLevel,
						icon = "info",
						type = "ok",
						title = "Many matches",
						message = sprintf(
							"%d matches found for \"%s\"\nHit 'Find' to jump to the next one.",
							nrow(matches),
							tclvalue(searchValue)
						)
					)
				}
				
				# Subset matches
				matches <- matches[ matchIterator ,]
			}
			
			# Coordinates update
			tcl(chromCombo, "set", as.character(matches$chrom))
			tclvalue(startValue) <- formatCoordinate((matches$start - 500e3) / 1e6)
			tclvalue(endValue) <- formatCoordinate((matches$end + 500e3) / 1e6)
			
			# Replot
			replot()	
		}
	}
	
	trackAction <- function() {
		# Call track manager
		drawables$fix.files(parent=topLevel)
		
		# Refresh combo lists from tracks
		refreshSearchCombo()
		refreshChromCombo()
	}
	
	
	
	## INTERFACE ##
	
	# Linux default style
	if(.Platform$OS.type == "unix") try(tcl("ttk::style", "theme", "use", "clam"), silent=TRUE)
	
	# Top level
	topLevel <- tktoplevel(class="Rgb")
	tktitle(topLevel) <- "Rgb - Genome browser"
	icon16 <- tcl("image", "create", "photo", file=system.file("cghRA_16x16.gif", package="Rgb"))
	icon32 <- tcl("image", "create", "photo", file=system.file("cghRA_32x32.gif", package="Rgb"))
	tcl("wm", "iconphoto", topLevel, "-default", icon16, icon32)
	
	# Maximization, if supported
	if(is(try(tkwm.state(topLevel, "zoomed"), silent=TRUE), "try-error")) {
		try(tcl("wm", "attributes", topLevel, zoomed=1), silent=TRUE)
	}
	
	# Horizontal resizing
	tkgrid.columnconfigure(topLevel, 1, weight=1)
	
		# Location main frame
		locationMainFrame <- tkframe(parent=topLevel)
			
			# Track frame
			trackFrame <- tkframe(parent=locationMainFrame, relief="groove", borderwidth=2, padx=5, pady=3)
				
				# Track button
				trackSelectButton <- tkbutton(parent=trackFrame, text="Tracks", command=trackAction, underline=0)
				tkgrid(trackSelectButton, column=1, row=1)
				
			tkgrid(trackFrame, column=1, row=1, padx=5, pady=5)
			
			# Position frame
			posFrame <- tkframe(parent=locationMainFrame, relief="groove", borderwidth=2, padx=5, pady=3)
				
				# "Chromosome"
				posLabel_1 <- tklabel(parent=posFrame, text="Chromosome")
				tkgrid(posLabel_1, column=1, row=1)
				
				# Chrom
				chromCombo <- ttkcombobox(parent=posFrame, values=character(0), width=3, justify="center", state="normal")
				tkgrid(chromCombo, column=2, row=1)
				
				# "from"
				posLabel_2 <- tklabel(parent=posFrame, text="from")
				tkgrid(posLabel_2, column=3, row=1)
				
				# Start
				startValue <- tclVar("")
				startEntry <- tkentry(parent=posFrame, width=12, textvariable=startValue, justify="center")
				tkgrid(startEntry, column=4, row=1)
				
				# "Mb to"
				posLabel_3 <- tklabel(parent=posFrame, text="Mb to")
				tkgrid(posLabel_3, column=5, row=1)
				
				# End
				endValue <- tclVar("")
				endEntry <- tkentry(parent=posFrame, width=12, textvariable=endValue, justify="center")
				tkgrid(endEntry, column=6, row=1)
				
				# "Mb"
				posLabel_4 <- tklabel(parent=posFrame, text="Mb")
				tkgrid(posLabel_4, column=7, row=1)
		
				# Jump button
				jumpButton <- tkbutton(parent=posFrame, text="Jump", command=replot, width=6, underline=0)
				tkgrid(jumpButton, column=8, row=1, padx=c(5, 0))
			
			tkgrid(posFrame, column=2, row=1, padx=5, pady=5)
			
			# Search frame
			searchFrame <- tkframe(parent=locationMainFrame, relief="groove", borderwidth=2, padx=5, pady=3)
				
				# Find button
				searchButton <- tkbutton(parent=searchFrame, text="Find", command=searchAction, width=6, underline=0)
				tkgrid(searchButton, column=1, row=1, padx=c(0, 5))
				
				# Value
				searchValue <- tclVar("")
				searchEntry <- tkentry(parent=searchFrame, width=10, textvariable=searchValue, justify="center")
				tkgrid(searchEntry, column=2, row=1)
				
				# "in"
				searchLabel_1 <- tklabel(parent=searchFrame, text="in")
				tkgrid(searchLabel_1, column=3, row=1)
				
				# Track to search
				searchCombo <- ttkcombobox(parent=searchFrame, values=character(0), width=25, justify="center", state="readonly")
				tkgrid(searchCombo, column=4, row=1)
				
				# Regexp
				searchRegexpValue <- tclVar("0")
				searchRegexpLabel <- tklabel(parent=searchFrame, text="using regexp")
				searchRegexpEntry <- tkcheckbutton(parent=searchFrame, variable=searchRegexpValue)
				tkgrid(searchRegexpLabel, column=5, row=1, padx=c(5, 0))
				tkgrid(searchRegexpEntry, column=6, row=1)
				
			tkgrid(searchFrame, column=3, row=1, padx=5, pady=5)
		
		tkgrid(locationMainFrame, column=1, row=1)
		
		# R-Plot widget
		if(is.na(hscale)) hscale <- autoHScale()
		if(is.na(vscale)) vscale <- autoVScale()
		plotWidget <- tkrplot(parent=topLevel, fun=plot.new, hscale=hscale, vscale=vscale)
		tkgrid(plotWidget, column=1, row=2)
	
	
	
	
	
	
	
	## LAUNCH ##
	
	# General events
	tkbind(topLevel, "<MouseWheel>", mouseWheel)             # Windows
	tkbind(topLevel, "<Button-4>", mouseWheelUp)             # Linux
	tkbind(topLevel, "<Button-5>", mouseWheelDown)           # Linux
	tkbind(topLevel, "<KeyPress-Up>", keyPressUp)
	tkbind(topLevel, "<KeyPress-Down>", keyPressDown)
	tkbind(topLevel, "<KeyPress-Left>", keyPressLeft)
	tkbind(topLevel, "<KeyPress-Right>", keyPressRight)
	tkbind(topLevel, "<KeyPress-r>", resize)
	tkbind(topLevel, "<KeyPress-f>", searchAction)
	tkbind(topLevel, "<KeyPress-j>", replot)
	tkbind(topLevel, "<KeyPress-t>", trackAction)
	
	# Plot region events
	tkbind(plotWidget, "<ButtonPress-1>", mousePress)
	tkbind(plotWidget, "<ButtonRelease-1>", mouseRelease)
	
	# Entry events
	tkbind(startEntry,  "<ButtonPress-3>", function(){ replaceEntry(startEntry) })
	tkbind(endEntry,    "<ButtonPress-3>", function(){ replaceEntry(endEntry) })
	tkbind(searchEntry, "<ButtonPress-3>", function(){ replaceEntry(searchEntry) })
	tkbind(startEntry,  "<KeyPress-Return>", replot)
	tkbind(endEntry,    "<KeyPress-Return>", replot)
	tkbind(searchEntry, "<KeyPress-Return>", searchAction)
	
	# Refresh combo lists from tracks
	refreshSearchCombo()
	refreshChromCombo()
	
	# Wait for closing
	if(isTRUE(blocking)) tkwait.window(topLevel)
	
	# Return drawable list for concurrent CLI update
	invisible(drawables)
}

