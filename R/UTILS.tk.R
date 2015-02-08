# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html
# Last update : 2013-08-06 11:45

# Interactive TCL-TK directory choosing
tk.folder = function(
		title = "Choose a directory",
		mustexist = TRUE,
		mandatory = TRUE
		)
	{
	suppressMessages(library(tcltk, quietly=TRUE))
	
	# Dialog
	suppressWarnings(folder <- tclvalue(tkchooseDirectory(title=title, mustexist=mustexist)))
	
	# No folder
	if(mandatory && folder == "") stop("No directory selected")
	
	return(folder)
}

# Interactive TCL-TK file choosing
tk.file <- function(title="Choose a file", typeNames="All files", typeExt="*", multiple=FALSE, mandatory=TRUE, type=c("open", "save"), initialdir=NULL, parent=NULL) {
	# Checks
	suppressMessages(library(tcltk, quietly=TRUE))
	type <- match.arg(type)
	
	# Ignore all warnings (fix for 'X11 BadDrawable')
	on.exit(options(warn=getOption("warn")))
	options(warn=-1)
	
	# Initial directory
	if(is.null(initialdir)) initialdir <- getOption("tk.currentDirectory", default=getwd())
	
	# File filters
	filetypes <- paste(sprintf("{{%s} {%s}}", typeNames, typeExt), collapse=" ")
	
	# Dialog
	if(is.null(parent)) {
		if(type == "open")        { files <- tkgetOpenFile(title=title, filetypes=filetypes, multiple=multiple, initialdir=initialdir)
		} else if(type == "save") { files <- tkgetSaveFile(title=title, filetypes=filetypes, initialdir=initialdir)
		}
	} else {
		if(type == "open")        { files <- tkgetOpenFile(parent=parent, title=title, filetypes=filetypes, multiple=multiple, initialdir=initialdir)
		} else if(type == "save") { files <- tkgetSaveFile(parent=parent, title=title, filetypes=filetypes, initialdir=initialdir)
		}
	}
	
	# Get value
	if(!multiple && as.integer(tcl("llength", files)) != 0L) { files <- tclvalue(files)
	} else                                                   { files <- as.character(files)
	}
	
	# No file
	if(mandatory && length(files) == 0) stop("No file selected")
	
	# Remind working directory
	if(length(files) > 0) options(tk.currentDirectory=dirname(files[1]))
	
	return(files)
}

# Interactive TCL-TK file choosing in multiple diretories
tk.files <- function(preselection=character(0), multiple=TRUE, parent=NULL, ...) {
	suppressMessages(library(tcltk, quietly=TRUE))
	
	## FUNCTIONS ##
	
	upCommand <- function() {
		toMove <- as.integer(tkcurselection(listBox))[1] + 1L
		if(toMove > 1) {
			# Update files
			tmp <- files[ toMove ]
			files[ toMove ] <<- files[ toMove - 1L ]
			files[ toMove - 1L ] <<- tmp
			
			# Update list
			tkdelete(listBox, 0, "end")
			for(f in files) tkinsert(listBox, "end", f)
			
			# Keep selection
			tcl(listBox, "selection", "set", toMove - 2L)
		}
	}
	
	downCommand <- function() {
		toMove <- as.integer(tkcurselection(listBox))[1] + 1L
		if(toMove < length(files)) {
			# Update files
			tmp <- files[ toMove ]
			files[ toMove ] <<- files[ toMove + 1L ]
			files[ toMove + 1L ] <<- tmp
			
			# Update list
			tkdelete(listBox, 0, "end")
			for(f in files) tkinsert(listBox, "end", f)
			
			# Keep selection
			tcl(listBox, "selection", "set", toMove)
		}
	}
	
	addCommand <- function() {
		for(f in tk.file(multiple=multiple, mandatory=FALSE, ...)) {
			tkinsert(listBox, "end", f)
			files <<- c(files, f)
		}
	}
	
	removeCommand <- function() {
		toRemove <- as.integer(tkcurselection(listBox))
		for(i in 1:length(toRemove)) {
			tkdelete(listBox, toRemove[i])
			files <<- files[-(toRemove[i]+1L)]
			toRemove[ toRemove > toRemove[i] ] <- toRemove[ toRemove > toRemove[i] ] - 1L
		}
	}
	
	doneCommand <- function() {
		tkdestroy(topLevel)
	}
	
	
	## INTERFACE ##
	
	# Top level
	topLevel <- tktoplevel()
	tktitle(topLevel) <- "Select files"
	
	# Make slave
	if(!is.null(parent)) {
		tcl("wm", "transient", topLevel, parent)
		tcl("wm", "withdraw", topLevel)
		tcl("wm", "deiconify", topLevel)
	}
	
		# Title
		listTitle <- tklabel(parent=topLevel, text="Current file selection")
		tkgrid(listTitle, column=1, row=1, pady=c(5,0))

		# List frame
		listFrame <- tkframe(parent=topLevel, padx=5, pady=3)
		tkgrid(listFrame, column=1, row=2, padx=5, pady=0, sticky="nsew")

			# Scroll bar
			scrollBar <- tkscrollbar(parent=listFrame, repeatinterval=5, command=function(...) { tkyview(listBox,...) })
			tkgrid(scrollBar, column=2, row=1, sticky="nsw", padx=c(1,0), pady=5)

			# List
			listBox <- tklistbox(parent=listFrame, height=10, width=70, selectmode="extended", yscrollcommand=function(...) { tkset(scrollBar, ...) }, background = "white")
			tkgrid(listBox, column=1, row=1, padx=c(0,1), pady=5, sticky="nsew")
			files <- preselection
			for(f in preselection) {
				tkinsert(listBox, "end", f)
			}

		# Button frame
		buttonsFrame <- tkframe(parent=topLevel, padx=5, pady=3)
		tkgrid(buttonsFrame, column=1, row=3, padx=5, pady=5)

			# Add button
			addButton <- tkbutton(parent=buttonsFrame, text="Add", width=10, command=addCommand)
			tkgrid(addButton, column=1, row=1, padx=5, pady=5)
			
			# Remove button
			removeButton <- tkbutton(parent=buttonsFrame, text="Remove", width=10, command=removeCommand)
			tkgrid(removeButton, column=2, row=1, padx=5, pady=5)
			
			# Up button
			upButton <- tkbutton(parent=buttonsFrame, text="Move up", width=10, command=upCommand)
			tkgrid(upButton, column=3, row=1, padx=5, pady=5)
			
			# Down button
			downButton <- tkbutton(parent=buttonsFrame, text="Move down", width=10, command=downCommand)
			tkgrid(downButton, column=4, row=1, padx=5, pady=5)
			
			# Done button
			doneButton <- tkbutton(parent=buttonsFrame, text="Done", width=10, command=doneCommand)
			tkgrid(doneButton, column=5, row=1, padx=c(25, 5), pady=5)
		
	# Resizable
	tkgrid.columnconfigure(topLevel, 1, weight=1)
	tkgrid.columnconfigure(listFrame, 1, weight=1)
	tkgrid.rowconfigure(topLevel, 2, weight=1)
	tkgrid.rowconfigure(listFrame, 1, weight=1)
	
	# End
	tkwait.window(topLevel)
	return(files)
}

# Custom silent condition handling function
# - Messages and warnings are silently caught (execution continues)
# - Errors are silently caught (execution stops)
# - NULL can be used to ignore a condition
handle <- function(
		expr,
		messageHandler,
		warningHandler,
		errorHandler
	) {
	toEval <- ""
	
	# Message handler
	if(!missing(messageHandler)) {
		if(is.null(messageHandler))            messageHandler <- function(m){}
		else if (!is.function(messageHandler)) stop("'messageHandler' must be a function or NULL")
		toEval <- sprintf("%s, message=function(m) { messageHandler(m); invokeRestart(\"muffleMessage\") }", toEval)
	}
	
	# Warning handler
	if(!missing(warningHandler)) {
		if(is.null(warningHandler))            warningHandler <- function(w){}
		else if (!is.function(warningHandler)) stop("'warningHandler' must be a function or NULL")
		toEval <- sprintf("%s, warning=function(w) { warningHandler(w); invokeRestart(\"muffleWarning\") }", toEval)
	}
	
	# Error handler
	if(!missing(errorHandler)) {
		if(is.null(errorHandler))            errorHandler <- function(e){}
		else if (!is.function(errorHandler)) stop("'errorHandler' must be a function or NULL")
		toEval <- sprintf("%s, error=function(e) { errorHandler(e); invokeRestart(\"muffleError\") }", toEval)
	}
	
	# Call execution
	eval(parse(text=sprintf("withRestarts(withCallingHandlers({expr}%s), muffleError=\"\")", toEval)))

	invisible(NULL)
}

