# R expressions for the track.bam$crawl() method, to counts cover depth in the genomic window
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

depth.init <- expression({
	output <- integer(end-start+1L)
	names(output) <- start:end
	n <- length(output)
})

depth.loop <- function(read, qMap=NA, qBase=NA, ...) {
	# Not sufficient mapping quality
	if(!is.na(qMap) && read$MAPQ < qMap) return(NULL)
	
	# Position in read sequence and in genome
	pRead <- 1L
	pGenome <- readStart <- read$POS
	readEnd <- read$.end
	
	# Loop on CIGAR operations
	opSizes <- read$CIGAR
	opTypes <- names(opSizes)
	for(o in 1:length(opSizes)) {
		opType <- opTypes[o]
		opSize <- opSizes[o]
		if(opType == "M" || opType == "=" || opType == "X") {
			# In SEQ and in reference
			# Cells to increment
			range <- pGenome - start + 1L:opSize
			cells <- range
			
			# Start filter
			if(readStart < start) { startFilter <- range > 0
			} else                { startFilter <- TRUE
			}
			
			# End filter
			if(readEnd > end) { endFilter <- range < n
			} else            { endFilter <- TRUE
			}
			
			# Quality filter
			if(!is.na(qBase)) {
				qual <- read$QUAL[ pRead + 1L:opSize - 1L ]
				qualFilter <- qual >= qBase
			} else {
				qualFilter <- TRUE
			}
			
			# Do filter
			cells <- cells[ startFilter & endFilter & qualFilter ]
			
			# Do increment (in crawl environment)
			if(length(cells) > 0L) output[ cells ] <<- output[ cells ] + 1L
			
			# Update positions
			pRead <- pRead + opSize
			pGenome <- pGenome + opSize
		} else if(opType == "D" || opType == "N") {
			# Not in SEQ but in reference
			pGenome <- pGenome + opSize
		} else if(opType == "I" || opType == "S") {
			# In SEQ but not in reference
			pRead <- pRead + opSize
		} ### Else "P", "H"
	}
	
	return(NULL)
}

depth.final <- expression()

