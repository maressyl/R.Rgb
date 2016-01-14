# R expressions for the track.bam$crawl() method, to get full read data in the genomic window
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

extract.init <- expression({
	output <- vector("list", 5000L)
})

extract.loop <- function(read, ...) {
	# Extend output list if necessary
	if(totalReads > length(output)) output <<- c(output, vector("list", 5000L))
	
	# Store whole read
	output[[ totalReads ]] <<- read
}

extract.final <- expression({
	# Remove unused read slots
	output <- output[ 1:totalReads ]
	
	# Apply S3 class
	attr(output, "bam") <- bamPath
	attr(output, "chrom") <- chrom
	attr(output, "start") <- start
	attr(output, "end") <- end
	class(output) <- "bamSlice"
})

# S3 method for BAI printing
print.bamSlice <- function(x, ...) {
	cat("Read list\n")
	cat("- BAM    : ", attr(x, "bam"), "\n", sep="")
	cat("- Region : ", attr(x, "chrom"), ":", attr(x, "start"), "-", attr(x, "end"), "\n", sep="")
	
	if(length(x) > 0) {
		reads <- x[[1]]$QNAME
		if(length(x) > 1) reads <- sprintf("%s ...", reads)
		reads <- sprintf(" (%s)", reads)
	} else { reads <- ""
	}
	cat("- Reads  : ", length(x), reads, "\n", sep="")
}

