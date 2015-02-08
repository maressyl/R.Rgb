# R expressions for the track.bam$crawl() method, to count each nucleotide type in the genomic window
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

pileup.init <- expression({
	alpha <- c("A", "C", "G", "T")
	output <- matrix(0L, nrow=end-start+1L, ncol=4, dimnames=list(start:end, alpha))
	n <- nrow(output)
})

pileup.loop <- function(read, qMap=NA, qBase=NA, ...) {
	# Not sufficient mapping quality
	if(!is.na(qMap) && read$MAPQ < qMap) return(NULL)
	
	# Position in read sequence and in genome
	pRead <- 1L
	pGenome <- readStart <- read$POS
	readEnd <- read$.end
	
	# Loop on CIGAR operations
	opSizes <- read$CIGAR
	opTypes <- names(opSizes)
	if(length(opSizes) > 0L) for(o in 1:length(opSizes)) {
		opType <- opTypes[o]
		opSize <- opSizes[o]
		if(opType == "M" || opType == "=" || opType == "X") {
			# In SEQ and in reference
			# Cells to increment
			range <- pGenome - start + 1L:opSize
			bases <- read$SEQ[ pRead + 1L:opSize - 1L ]
			cells <- cbind(range, match(bases, alpha))
			
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
			cells <- cells[ startFilter & endFilter & qualFilter , , drop=FALSE ]
			
			# Do increment (in crawl environment)
			if(nrow(cells) > 0L) output[ cells ] <<- output[ cells ] + 1L
			
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

pileup.final <- expression()

