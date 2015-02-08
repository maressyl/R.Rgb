# Interactive TCL-TK object selection from the global environment
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

tk.memory <- function(
		parent = NULL
	) {
	suppressMessages(library(tcltk, quietly=TRUE))
	
	### FIX ### silence R CMD CHECK 'NOTE'
	tableFrame <- NULL
	
	# Find objects in global environment
	drawCalls <- findDrawables()
	
	# Get meta-data
	drawCount <- length(drawCalls)
	drawNames <- character(0)
	drawClasses <- character(0)
	drawIDs <- character(0)
	for(drawCall in drawCalls) {
		var <- eval(parse(text=drawCall), envir=attr(drawCalls, "envir"))
		drawNames <- c(drawNames, var$name)
		drawClasses <- c(drawClasses, class(var))
		drawIDs <- c(drawIDs, sub("^<environment: (.+)>$", "\\1", capture.output(as.environment(var))))
	}
	
	
	output <- character(0)
	
	addCommand <- function() {
		# Get checkbox results
		drawSelected <- logical(0)
		if(drawCount > 0) for(i in 1:drawCount) drawSelected[i] <- as.logical(as.integer(tclvalue(selectVarList[[i]])))
		
		# Filter output list
		tmp <- drawCalls[ drawSelected ]
		attr(tmp, "envir") <- attr(drawCalls, "envir")
		output <<- tmp
		
		tkdestroy(topLevel)
	}
	
	cancelCommand <- function() {
		tkdestroy(topLevel)
	}
	
	
	# Top level
	topLevel <- tktoplevel()
	tktitle(topLevel) <- "Rgb - Drawables in memory"
	tkgrid.columnconfigure(topLevel, 1, weight=1)
	tkgrid.rowconfigure(topLevel, 2, weight=1)

		# Table frame
		tableFrame <- tkframe(parent=topLevel)
			
			# Call column
			callLabel <- tklabel(parent=tableFrame, text="Call", background="#888888", padx=5)
			callList <- list()
			tkgrid(callLabel, column=1, row=1, sticky="nswe", padx=1, pady=1)
			if(drawCount > 0) for(i in 1:drawCount) {
				callList[[i]] <- tklabel(parent=tableFrame, text=drawCalls[i], background="#CCCCCC", padx=5, anchor="w")
				tkgrid(callList[[i]], column=1, row=i+1, padx=1, pady=1, sticky="nswe")
			}
			
			# Name column
			nameLabel <- tklabel(parent=tableFrame, text="Name", background="#888888", padx=5)
			nameList <- list()
			tkgrid(nameLabel, column=2, row=1, sticky="nswe", padx=1, pady=1)
			if(drawCount > 0) for(i in 1:drawCount) {
				nameList[[i]] <- tklabel(parent=tableFrame, text=drawNames[i], background="#CCCCCC", padx=5)
				tkgrid(nameList[[i]], column=2, row=i+1, padx=1, pady=1, sticky="nswe")
			}
			
			# Class column
			classLabel <- tklabel(parent=tableFrame, text="Class", background="#888888", padx=5)
			classList <- list()
			tkgrid(classLabel, column=3, row=1, sticky="nswe", padx=1, pady=1)
			if(drawCount > 0) for(i in 1:drawCount) {
				classList[[i]] <- tklabel(parent=tableFrame, text=drawClasses[i], background="#CCCCCC", padx=5)
				tkgrid(classList[[i]], column=3, row=i+1, padx=1, pady=1, sticky="nswe")
			}
			
			# ID column
			idLabel <- tklabel(parent=tableFrame, text="ID", background="#888888", padx=5)
			idList <- list()
			tkgrid(idLabel, column=4, row=1, sticky="nswe", padx=1, pady=1)
			if(drawCount > 0) for(i in 1:drawCount) {
				idList[[i]] <- tklabel(parent=tableFrame, text=drawIDs[i], background="#CCCCCC", padx=5)
				tkgrid(idList[[i]], column=4, row=i+1, padx=1, pady=1, sticky="nswe")
			}
			
			# Select column
			selectLabel <- tklabel(parent=tableFrame, text="Select", background="#888888", padx=5)
			selectVarList <- list()
			selectWidgetList <- list()
			tkgrid(selectLabel, column=5, row=1, sticky="nswe", padx=1, pady=1)
			if(drawCount > 0) for(i in 1:drawCount) {
				selectVarList[[i]] <- tclVar("0")
				selectWidgetList[[i]] <- tkcheckbutton(parent=tableFrame, variable=selectVarList[[i]])
				tkgrid(selectWidgetList[[i]], column=5, row=i+1, padx=1, pady=1)
			}
		
		tkgrid(tableFrame, column=1, row=1, sticky="nsew", padx=5, pady=5)
		tkgrid.columnconfigure(tableFrame, 1, weight=1)

		# Button frame
		buttonFrame <- tkframe(parent=topLevel)
		
			# Action buttons
			addButton <- tkbutton(parent=buttonFrame, text="Add object(s)", command=addCommand)
			cancelButton <- tkbutton(parent=buttonFrame, text="Cancel", command=cancelCommand)
			tkgrid(addButton, column=1, row=1, padx=5, pady=5)
			tkgrid(cancelButton, column=2, row=1, padx=5, pady=5)
		
		tkgrid(buttonFrame, column=1, row=3, pady=c(10,0))
	
	# End
	tkwait.window(topLevel)
	
	return(output)
}

