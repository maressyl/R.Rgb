# Interactive TCL-TK genome browser
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

tk.browse <- function(
		drawables = drawable.list(),
		blocking = FALSE,
		updateLimit = 0.4,
		render = c("auto", "png", "tkrplot"),
		tkrplot.scale = 1,
		png.res = 100,
		png.file = tempfile(fileext=".png")
		)
	{
	# Check tracks
	if(!is(drawables, "drawable.list")) stop("'drawables' must be a 'drawable.list' object")
	drawables$check(warn=FALSE)
	
	# Check renderer
	render <- match.arg(render)
	tcltkVer <- as.double(sub("^([0-9]+\\.[0-9]+).+$", "\\1", tcltk::tclVersion()))
	if(tcltkVer >= 8.6) {
		# PNG is not compatible
		if(render == "auto") render <- "png"
	} else {
		# PNG is compatible
		if(render == "auto")       { render <- "tkrplot"
		} else if(render == "png") { stop("PNG rendering requires tcltk 8.6 or above")
		}
	}
	
	# Check if tkrplot is installed
	if(length(find.package("tkrplot", quiet=TRUE)) == 0L) {
		tcltk::tkmessageBox(
			parent = topLevel,
			icon = "info",
			type = "ok",
			title = "tkrplot package missing",
			message = "The optional 'tkrplot' package is required to use tk.browse(), please install it and try again."
		)
	}
	
	
	
	## FUNCTIONS ##
	
	checkPlot <- function(silent=FALSE) {
		# No track
		if(drawables$count == 0) {
			if(!silent) {
				tcltk::tkmessageBox(
					parent = topLevel,
					icon = "info",
					type = "ok",
					title = "Choose drawables",
					message = "Use the 'Tracks' button to add drawable objects to the plot."
				)
			}
			return(FALSE)
		}
		
		# 'chrom' error
		if(tcltk::tclvalue(tcltk::tcl(chromCombo, "get")) == "") {
			if(!silent) {
				tcltk::tkmessageBox(
					parent = topLevel,
					icon = "info",
					type = "ok",
					title = "Choose chromosome",
					message = "Enter a chromosome name in the 'Chromosome' field, or use 'Find'."
				)
			}
			return(FALSE)
		}
		
		# 'start' or 'end' error
		if(!grepl("^(-?[0-9]+(\\.[0-9]+)?)?$", tcltk::tclvalue(startValue)) || !grepl("^(-?[0-9]+(\\.[0-9]+)?)?$", tcltk::tclvalue(endValue))) {
			if(!silent) {
				tcltk::tkmessageBox(
					parent = topLevel,
					icon = "error",
					type = "ok",
					title = "Invalid coordinates",
					message = "Enter numeric coordinates, using the dot as decimal separator."
				)
			}
			return(FALSE)
		}
		
		# No error
		return(TRUE)
	}
	
	# Compute current plot area height
	autoHeight <- function(unit) {
		if(unit == "px")           { out <- max(300L, as.integer(tcltk::tclvalue(tcltk::tkwinfo("height", plotFrame))))
		} else if(unit == "scale") { out <- autoHeight("px") / scaleFactor * tkrplot.scale
		}
		
		return(out)
	}
	
	# Compute current plot area width, in pixels
	autoWidth <- function(unit) {
		if(unit == "px")           { out <- max(300L, as.integer(tcltk::tclvalue(tcltk::tkwinfo("width", plotFrame))))
		} else if(unit == "scale") { out <- autoWidth("px") / scaleFactor * tkrplot.scale
		}
		
		return(out)
	}
	
	resize <- function() {
		# Automatic sizes
		if(render == "tkrplot") {
			vscale <<- autoHeight("scale")
			hscale <<- autoWidth("scale")
		} else if(render == "png") {
			height <<- autoHeight("px")
			width <<- autoWidth("px")
		}
		
		# Refresh
		replot()
	}
	
	savePar <- list()
	xConvert <- function(x) {
		# Plot area coordinates in pixels
		width <- as.numeric(tcltk::tclvalue(tcltk::tkwinfo("reqwidth", plotWidget)))
		xMin <- savePar$plt[1] * width
		xMax <- savePar$plt[2] * width
		
		# From pixel area to x range
		return((x - xMin) / (xMax - xMin) * (savePar$usr[2] - savePar$usr[1]) + savePar$usr[1])
	}
	
	keyPressUp <- function() {
		zoom("in")
	}
	
	keyPressDown <- function() {
		zoom("out")
	}
	
	keyPressLeft <- function() {
		move("left")
	}
	
	keyPressRight <- function() {
		move("right")
	}
	
	dragFrom <- NA
	mousePress <- function(x, y) {
		dragFrom <<- formatCoordinate(xConvert(as.double(x)) / 1e6)
	}
	
	dragTo <- NA
	mouseRelease <- function(x, y) {
		dragTo <<- formatCoordinate(xConvert(as.double(x)) / 1e6)
		if(dragFrom != dragTo) {
			tcltk::tclvalue(startValue) <- min(as.numeric(dragFrom), as.numeric(dragTo))
			tcltk::tclvalue(endValue) <- max(as.numeric(dragFrom), as.numeric(dragTo))
			replot()
		}
	}
	
	mouseWheel <- function(D) {
		if(D > 0) { mouseWheelUp()
		} else    { mouseWheelDown()
		}
	}
	
	mouseWheelUp <- function() {
		zoom("in")
	}
	
	mouseWheelDown <- function() {
		zoom("out")
	}
	
	replaceEntry <- function(widget) {
		tcltk::tkfocus("-force", widget)
		tcltk::tcl(widget, "delete", "0", "end")
		tcltk::tcl(widget, "icursor", "end")
	}
	
	# browsePlot() call to produce the plot
	plot.core <- function() {
		graphics::par(bg="#FFFFFF")
		savePar <<- browsePlot(
			drawables = drawables,
			chrom = as.character(tcltk::tcl(chromCombo, "get")),
			start = as.double(tcltk::tclvalue(startValue)) * 1e6,
			end = as.double(tcltk::tclvalue(endValue)) * 1e6
		)
	}
	
	# Welcome screen
	plot.empty <- function() {
		graphics::par(bg="#FFFFFF", mar=c(0,0,0,0))
		graphics::plot(x=NA, y=NA, xlim=0:1, ylim=0:1, xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
		graphics::text(x=0.5, y=0.5, labels="Welcome to Rgb !\n\n1. Click \"Tracks\" to select data files to visualize.\n2. Enter genomic coordinates and click \"Jump\".")
	}
	
	# Replot using 'png' rendered
	plot.png <- function(empty) {
		# Produce image file
		grDevices::png(png.file, width=width, height=height, res=png.res)
		if(isTRUE(empty)) { plot.empty()
		} else            { plot.core()
		}
		grDevices::dev.off()
		
		# Refresh image
		tcltk::tkconfigure(plotImage, file=png.file, width=width, height=height)
		tcltk::tkconfigure(plotWidget, width=width, height=height)
	}
	
	# Replot using 'tkrplot' rendered
	plot.tkrplot <- function(empty) {
		tkrplot::tkrreplot(
			lab = plotWidget,
			fun = if(isTRUE(empty)) { plot.empty } else { plot.core },
			hscale = hscale,
			vscale = vscale
		)
	}
	
	# Replot common workflow
	replot <- function(empty=FALSE) {
		# Check coordinates
		if(!isTRUE(empty)) empty <- !checkPlot()
		
		# Cursor
		tcltk::tkconfigure(topLevel, cursor="watch")
		tcltk::.Tcl("update idletasks")
		
		# Grab focus to avoid keyboard shortcuts quirks
		tcltk::tkfocus(plotWidget)
		
		# Replot
		handle(
			expr = {
				if(render == "png")            { plot.png(empty=empty)
				} else if(render == "tkrplot") { plot.tkrplot(empty=empty)
				}
			},
			# Silently ignore message()
			messageHandler = NULL,
			# Pass warning() but continue execution
			warningHandler = function(w) {
				tcltk::tkmessageBox(
					parent = topLevel,
					icon = "warning",
					type = "ok",
					title = "Warning in browsePlot()",
					message = conditionMessage(w)
				)
			},
			# Pass stop() and stop execution
			errorHandler = function(e) {
				tcltk::tkmessageBox(
					parent = topLevel, 
					icon = "error",
					type = "ok",
					title = "Error in browsePlot()",
					message = conditionMessage(e)
				)
			}					
		)
		
		# Cursor
		tcltk::tkconfigure(topLevel, cursor="arrow")
		tcltk::.Tcl("update idletasks")
	}
	
	forceCoordinates <- function() {
		# Is forcing needed ?
		if(tcltk::tclvalue(startValue) == "" || tcltk::tclvalue(endValue) == "") {
			# Needed but impossible
			if(! "usr" %in% names(savePar) || ! "chrom" %in% names(savePar) || as.character(tcltk::tcl(chromCombo, "get")) != savePar$chrom) {
				return(FALSE)
			}
			
			# Computation from previous plot
			if(tcltk::tclvalue(startValue) == "") tcltk::tclvalue(startValue) <- formatCoordinate(savePar$usr[1] / 1e6)
			if(tcltk::tclvalue(endValue) == "")   tcltk::tclvalue(endValue) <- formatCoordinate(savePar$usr[2] / 1e6)
		}
		
		return(TRUE)
	}
	
	formatCoordinate <- function(x) {
		x <- as.numeric(x)
		if(abs(x) < 0.000003) x <- 0
		x <- sprintf("%.6f", round(x, 6))
		x <- sub("\\.?0+$", "", x)
		return(x)
	}
	
	updateT0 <- updateT1 <- proc.time()['elapsed']
	
	move <- function(way) {
		updateT1 <<- proc.time()['elapsed']
		if(updateT1 - updateT0 > updateLimit) {
			if(checkPlot()) {
				if(forceCoordinates()) {
					# Values
					LFactor = 2
					LStart = as.double(tcltk::tclvalue(startValue))
					LEnd = as.double(tcltk::tclvalue(endValue))
				
					# Coordinate update
					width = (LEnd - LStart) / LFactor
					if (way == "left") { width = -width }
					tcltk::tclvalue(startValue) <- formatCoordinate(LStart + width)
					tcltk::tclvalue(endValue) <- formatCoordinate(LEnd + width)
					
					# Refresh timer
					updateT0 <<- updateT1
				}
			
				# Replot
				replot()
			}
		}
	}
	
	zoom <- function(way) {
		updateT1 <<- proc.time()['elapsed']
		if(updateT1 - updateT0 > updateLimit) {
			if(checkPlot()) {
				if(forceCoordinates()) {
					# Values
					LFactor = 2
					LStart = as.double(tcltk::tclvalue(startValue))
					LEnd = as.double(tcltk::tclvalue(endValue))
				
					# Inversion
					modifier = if (way == "out") { -1 } else { LFactor }
				
					# Coordinates update
					width = (LEnd - LStart) * (LFactor-1) / modifier / 2
					tcltk::tclvalue(startValue) <- formatCoordinate(LStart + width)
					tcltk::tclvalue(endValue) <- formatCoordinate(LEnd - width)
					
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
						tcltk::tkmessageBox(
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
		tcltk::tkconfigure(chromCombo, values=chromList)
	}
	
	refreshSearchCombo <- function() {
		values <- sub("\\.[^\\.]+$", "", drawables$names)
		tcltk::tkconfigure(searchCombo, values=values)
		if(length(values) > 0) tcltk::tcl(searchCombo, "set", values[1])
	}
	
	matchIterator <- 0L
	searchAction <- function() {
		track <- drawables$get( as.integer(tcltk::tcl(searchCombo, "current")) + 1L )
		if(tcltk::tclvalue(searchValue) == "") {
			tcltk::tkmessageBox(
				parent = topLevel,
				icon = "info",
				type = "ok",
				title = "Choose a target",
				message = "Enter in the next field the name of the element to search, or a regular expression."
			)
		} else if(!is(track, "track.table")) {
			tcltk::tkmessageBox(
				parent = topLevel,
				icon = "error",
				type = "ok",
				title = "Unable to find the target",
				message = "The track you are searching is not a 'track.table' inheriting R object."
			)
		} else {
			# Extraction
			regexp <- as.logical(as.integer(tcltk::tclvalue(searchRegexpValue)))
			if(regexp) i <- sprintf("grep(pattern=\"%s\", x=name, value=FALSE, ignore.case=TRUE)", tcltk::tclvalue(searchValue))
			else       i <- sprintf("name == \"%s\"", tcltk::tclvalue(searchValue))
			matches <- suppressWarnings(track$extract(parse(text=i)))
			
			if(nrow(matches) == 0) {
				# No match
				tcltk::tkmessageBox(
					parent = topLevel,
					icon = "warning",
					type = "ok",
					title = "Unable to find the target",
					message = sprintf("No match for \"%s\"", tcltk::tclvalue(searchValue))
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
					tcltk::tkmessageBox(
						parent = topLevel,
						icon = "info",
						type = "ok",
						title = "Multiple matches",
						message = sprintf(
							"%d matches found for \"%s\"\nHit 'Find' to jump to the next one.\n\n%s",
							nrow(matches),
							tcltk::tclvalue(searchValue),
							paste(matchList, collapse="\n")
						)
					)
				} else {
					tcltk::tkmessageBox(
						parent = topLevel,
						icon = "info",
						type = "ok",
						title = "Many matches",
						message = sprintf(
							"%d matches found for \"%s\"\nHit 'Find' to jump to the next one.",
							nrow(matches),
							tcltk::tclvalue(searchValue)
						)
					)
				}
				
				# Subset matches
				matches <- matches[ matchIterator ,]
			}
			
			# Coordinates update
			tcltk::tcl(chromCombo, "set", as.character(matches$chrom))
			tcltk::tclvalue(startValue) <- formatCoordinate((matches$start - 500e3) / 1e6)
			tcltk::tclvalue(endValue) <- formatCoordinate((matches$end + 500e3) / 1e6)
			
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
	if(.Platform$OS.type == "unix") try(tcltk::tcl("ttk::style", "theme", "use", "clam"), silent=TRUE)
	
	# Top level
	topLevel <- tcltk::tktoplevel(class="Rgb")
	tcltk::tktitle(topLevel) <- "Rgb - Genome browser"
	icon16 <- tcltk::tcl("image", "create", "photo", file=system.file("cghRA_16x16.gif", package="Rgb"))
	icon32 <- tcltk::tcl("image", "create", "photo", file=system.file("cghRA_32x32.gif", package="Rgb"))
	tcltk::tcl("wm", "iconphoto", topLevel, "-default", icon16, icon32)
	
	# Maximization, if supported
	if(is(try(tcltk::tkwm.state(topLevel, "zoomed"), silent=TRUE), "try-error")) {
		try(tcltk::tcl("wm", "attributes", topLevel, zoomed=1), silent=TRUE)
	}
	
	# Horizontal resizing
	tcltk::tkgrid.columnconfigure(topLevel, 1, weight=1)
	tcltk::tkgrid.rowconfigure(topLevel, 2, weight=1)
	
		# Location main frame
		locationMainFrame <- tcltk::tkframe(parent=topLevel)
			
			# Track frame
			trackFrame <- tcltk::tkframe(parent=locationMainFrame, relief="groove", borderwidth=2, padx=5, pady=3)
				
				# Track button
				trackSelectButton <- tcltk::tkbutton(parent=trackFrame, text="Tracks", command=trackAction, underline=0)
				tcltk::tkgrid(trackSelectButton, column=1, row=1)
				
			tcltk::tkgrid(trackFrame, column=1, row=1, padx=5, pady=5)
			
			# Position frame
			posFrame <- tcltk::tkframe(parent=locationMainFrame, relief="groove", borderwidth=2, padx=5, pady=3)
				
				# "Chromosome"
				posLabel_1 <- tcltk::tklabel(parent=posFrame, text="Chromosome")
				tcltk::tkgrid(posLabel_1, column=1, row=1)
				
				# Chrom
				chromCombo <- tcltk::ttkcombobox(parent=posFrame, values=character(0), width=3, justify="center", state="normal")
				tcltk::tkgrid(chromCombo, column=2, row=1)
				
				# "from"
				posLabel_2 <- tcltk::tklabel(parent=posFrame, text="from")
				tcltk::tkgrid(posLabel_2, column=3, row=1)
				
				# Start
				startValue <- tcltk::tclVar("")
				startEntry <- tcltk::tkentry(parent=posFrame, width=12, textvariable=startValue, justify="center")
				tcltk::tkgrid(startEntry, column=4, row=1)
				
				# "Mb to"
				posLabel_3 <- tcltk::tklabel(parent=posFrame, text="Mb to")
				tcltk::tkgrid(posLabel_3, column=5, row=1)
				
				# End
				endValue <- tcltk::tclVar("")
				endEntry <- tcltk::tkentry(parent=posFrame, width=12, textvariable=endValue, justify="center")
				tcltk::tkgrid(endEntry, column=6, row=1)
				
				# "Mb"
				posLabel_4 <- tcltk::tklabel(parent=posFrame, text="Mb")
				tcltk::tkgrid(posLabel_4, column=7, row=1)
		
				# Jump button
				jumpButton <- tcltk::tkbutton(parent=posFrame, text="Jump", command=replot, width=6, underline=0)
				tcltk::tkgrid(jumpButton, column=8, row=1, padx=c(5, 0))
			
			tcltk::tkgrid(posFrame, column=2, row=1, padx=5, pady=5)
			
			# Search frame
			searchFrame <- tcltk::tkframe(parent=locationMainFrame, relief="groove", borderwidth=2, padx=5, pady=3)
				
				# Find button
				searchButton <- tcltk::tkbutton(parent=searchFrame, text="Find", command=searchAction, width=6, underline=0)
				tcltk::tkgrid(searchButton, column=1, row=1, padx=c(0, 5))
				
				# Value
				searchValue <- tcltk::tclVar("")
				searchEntry <- tcltk::tkentry(parent=searchFrame, width=10, textvariable=searchValue, justify="center")
				tcltk::tkgrid(searchEntry, column=2, row=1)
				
				# "in"
				searchLabel_1 <- tcltk::tklabel(parent=searchFrame, text="in")
				tcltk::tkgrid(searchLabel_1, column=3, row=1)
				
				# Track to search
				searchCombo <- tcltk::ttkcombobox(parent=searchFrame, values=character(0), width=25, justify="center", state="readonly")
				tcltk::tkgrid(searchCombo, column=4, row=1)
				
				# Regexp
				searchRegexpValue <- tcltk::tclVar("0")
				searchRegexpLabel <- tcltk::tklabel(parent=searchFrame, text="using regexp")
				searchRegexpEntry <- tcltk::tkcheckbutton(parent=searchFrame, variable=searchRegexpValue)
				tcltk::tkgrid(searchRegexpLabel, column=5, row=1, padx=c(5, 0))
				tcltk::tkgrid(searchRegexpEntry, column=6, row=1)
				
			tcltk::tkgrid(searchFrame, column=3, row=1, padx=5, pady=5)
		
		tcltk::tkgrid(locationMainFrame, column=1, row=1)
		
		# Plot frame
		plotFrame <- tcltk::tkframe(parent=topLevel)
		tcltk::tkgrid(plotFrame, column=1, row=2, sticky="nsew")
		
			# R-Plot widget (wait for maximization to apply and propagate)
			tcltk::.Tcl("update idletasks")
			
			if(render == "png") {
				# Default size
				height <- autoHeight("px")
				width <- autoWidth("px")
				
				# Display (empty) PNG image
				plotImage <- tcltk::tkimage.create("photo", width=width, height=height)
				plotWidget <- tcltk::tkcanvas(plotFrame, width=width, height=height)
				tcltk::tkcreate(plotWidget, "image", 0, 0, anchor="nw", image=plotImage)
			} else if(render == "tkrplot") {
				# tkrplot widget
				plotWidget <- tkrplot::tkrplot(parent=plotFrame, fun=plot.empty, hscale=1, vscale=1)
			}
		
			tcltk::tkgrid(plotWidget, column=1, row=1)
	
	# Guess scale factor from 1 x 1 empty plot
	if(render == "tkrplot") {
		# Scale factor (wait for plot update)
		tcltk::.Tcl("update idletasks")
		scaleFactor <- as.integer(tcltk::tclvalue(tcltk::tkwinfo("width", plotWidget)))
		if(scaleFactor < 100) stop("Scale factor detection seems to have failed (", scaleFactor, ")")
		
		# Default size
		vscale <- autoHeight("scale")
		hscale <- autoWidth("scale")
		
		replot(empty=TRUE)
	} else {
		# Welcome screen
		replot(empty=TRUE)
	}
	
	
	
	
	
	## LAUNCH ##
	
	# General events
	tcltk::tkbind(topLevel, "<MouseWheel>", mouseWheel)          # Windows
	tcltk::tkbind(topLevel, "<Button-4>", mouseWheelUp)          # Linux
	tcltk::tkbind(topLevel, "<Button-5>", mouseWheelDown)        # Linux
	tcltk::tkbind(topLevel, "<KeyPress-Up>", keyPressUp)
	tcltk::tkbind(topLevel, "<KeyPress-Down>", keyPressDown)
	tcltk::tkbind(topLevel, "<KeyPress-Left>", keyPressLeft)
	tcltk::tkbind(topLevel, "<KeyPress-Right>", keyPressRight)
	tcltk::tkbind(topLevel, "<KeyPress-r>", resize)
	tcltk::tkbind(topLevel, "<KeyPress-f>", searchAction)
	tcltk::tkbind(topLevel, "<KeyPress-j>", replot)
	tcltk::tkbind(topLevel, "<KeyPress-t>", trackAction)
	
	# Plot region events
	tcltk::tkbind(plotWidget, "<ButtonPress-1>", mousePress)
	tcltk::tkbind(plotWidget, "<ButtonRelease-1>", mouseRelease)
	
	# Entry events
	tcltk::tkbind(startEntry,  "<ButtonPress-3>", function(){ replaceEntry(startEntry) })
	tcltk::tkbind(endEntry,    "<ButtonPress-3>", function(){ replaceEntry(endEntry) })
	tcltk::tkbind(searchEntry, "<ButtonPress-3>", function(){ replaceEntry(searchEntry) })
	tcltk::tkbind(startEntry,  "<KeyPress-Return>", replot)
	tcltk::tkbind(endEntry,    "<KeyPress-Return>", replot)
	tcltk::tkbind(searchEntry, "<KeyPress-Return>", searchAction)
	
	# Refresh combo lists from tracks
	refreshSearchCombo()
	refreshChromCombo()
	
	# Wait for closing
	if(isTRUE(blocking)) tcltk::tkwait.window(topLevel)
	
	# Return drawable list for concurrent CLI update
	invisible(drawables)
}

