\name{draw.bg}
\alias{draw.bg}
\title{
  Background for track plots
}

\description{
  This function draws the background for the other track ploting functions.
}

\usage{
  draw.bg(start, end, ylab = "", ysub = as.character(NA), mar = c(0.2, 5, 0.2, 1),
    xaxt = "s", yaxt = "n", yaxs = "r", ylim = c(0, 1), cex.lab = 1, bty = "o", ...)
}

\arguments{
  \item{start}{
    Single integer value, the genomic position of the window's left boundary.
}
  \item{end}{
    Single integer value, the genomic position of the window's right boundary.
}
  \item{ylab}{
    The name of the Y axis. See \code{par}.
}
  \item{ysub}{
    Similar to \code{ylab}, but written on a closer line when \code{yxat} is "n". Can be \code{NA} to disable it.
}
  \item{mar}{
    A numerical vector of the form "c(bottom, left, top, right)" which gives the number of lines of margin to be specified on the four sides of the plot. See \code{par}.
}
  \item{xaxt}{
    Whether to plot an X axis ("s") or not ("n"). See \code{par}.
}
  \item{yaxt}{
    Whether to plot an Y axis ("s") or not ("n"). If no Y axis is drawn, \code{ysub} may be used to plot a sub-title to the Y axis. See \code{par}.
}
  \item{yaxs}{
    Y axis style, "r" enlarges the Y limits by 4 percents on each side for a cleaner look, "i" will not. See \code{par}.
}
  \item{ylim}{
    The Y axis limits as a numerical vector of the form "c(start, end)" of the plot. Note that start > end is allowed and leads to a "reversed axis". Use "NULL" to guess the axis range from the data. See \code{plot.default}.
}
  \item{cex.lab}{
    The relative character size of x and y axis labels (default: 1). See \code{par}.
}
  \item{bty}{
    A character string which determined the type of box which is drawn about plots. If bty is one of "o" (the default), "l", "7", "c", "u", or "]" the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box. See \code{par}.
}
  \item{\dots}{
    Not used, only ignores other arguments.
}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{draw.boxes}}, \code{\link{draw.hist}}, \code{\link{draw.points}}, \code{\link{draw.bam}}, \code{\link{draw.fasta}}
}

