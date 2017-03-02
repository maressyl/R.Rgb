# Moves bigWigToBedGraph file to the package location for straight bigWigToBedGraph usage
# http://hgdownload.soe.ucsc.edu/admin/exe/
# Author : sylvain.mareschal@etu.univ-rouen.fr
bigWigInstall = function(
		bigWigToBedGraph,
		cygwin
		)
	{
	# Directory
	package = find.package("Rgb", quiet=TRUE, verbose=FALSE)
	dir.create(sprintf("%s/bigWigToBedGraph", package), showWarnings=FALSE)
	
	# Cygwin DLL
	if(.Platform$OS.type == "windows") {
		# Windows
		if(missing(cygwin)) {
			warning("'cygwin' missing and required, unless it is installed")
		} else {
			# Check Cygwin file
			if(basename(cygwin) != "cygwin1.dll") stop("'cygwin' file should be named \"cygwin1.dll\"")
			if(!file.exists(cygwin))              stop("'cygwin' file not found")
			
			# Copy Cygwin file
			status <- !suppressWarnings(file.copy(cygwin, sprintf("%s/bigWigToBedGraph/cygwin1.dll", package), overwrite=TRUE))
			if(!status)                           stop("'cygwin' copy failed at file system level (missing space, permission...)")
			
			# Done
			message("'cygwin' file is valid and installed")
		}
	} else {
		# UNIX
		if(missing(cygwin)) { message("'cygwin' file is missing but not required")
		} else              { message("'cygwin' file is provided but not required")
		}
	}
	
	# Check bigWigToBedGraph file
	if(missing(bigWigToBedGraph))      stop("'bigWigToBedGraph' missing and required")
	if(!file.exists(bigWigToBedGraph)) stop("'bigWigToBedGraph' file not found")
	
	# Copy bigWigToBedGraph file
	status <- !suppressWarnings(file.copy(bigWigToBedGraph, sprintf("%s/bigWigToBedGraph/bigWigToBedGraph.exe", package), overwrite=TRUE))
	if(!status)                        stop("'bigWigToBedGraph' copy failed at file system level (missing space, permission...)")
	
	# Done
	message("'bigWigToBedGraph' file is valid and installed")
}

