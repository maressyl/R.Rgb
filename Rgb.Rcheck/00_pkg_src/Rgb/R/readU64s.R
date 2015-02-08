# Reads multiple unsigned 64 bit integer (stored as double)
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

readU64s <- function(con, n) {
	if(n > 0) {
		bytes <- .Internal(readBin(con=con, what="integer", n=n*4L, size=2L, signed=FALSE, swap=FALSE))
		bytes <- matrix(bytes, nrow=4L)
		bytes <- bytes * 65536L^(0:3)
		output <- .Internal(colSums(matrix=bytes, nrow=4L, ncol=n, na.rm=FALSE))
		return(output)
	} else {
		return(double(0))
	}
}

