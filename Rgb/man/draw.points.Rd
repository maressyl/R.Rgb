\name{draw.points}
\alias{draw.points}
\title{
  Scatter plot of a track
}

\description{
  This function draws a slice of a track content, with a distinct point for each track element.
}

\usage{
  draw.points(slice, start, end, column = "value", colorVal = "#666666",
    colorFun = function() NULL, cex.lab = 1, cex = 0.6, pch = "+", bty = "o", ...)
}

\arguments{
  \item{slice}{
    A \code{data.frame} holding the data to plot, with elements in rows and data in columns.
}
  \item{start}{
    Single integer value, the left boundary of the window, in base pairs.
}
  \item{end}{
    Single integer value, the right boundary of the window, in base pairs.
}
  \item{column}{
    Single character value, the name of the \code{slice} column to use for bar heights.
}
  \item{colorVal}{
    The color to fill bars with (as a name, an integer or an hexadecimal character description).
}
  \item{colorFun}{
    A function with no arguments, which returns a vector of as many colors than the \code{slice} has rows. It can make use of any argument described on this page (including custom arguments passed via "..."), as its enclosing environment is redefined to the calling one. \code{colorVal} must be \code{NA} for the function to be used.
}
  \item{cex.lab}{
    See \code{par}.
}
  \item{cex}{
    See \code{par}.
}
  \item{pch}{
    See \code{par}.
}
  \item{bty}{
    See \code{par}.
}
  \item{\dots}{
    Further arguments to be passed to \code{\link{draw.bg}}.
}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{draw.bg}}, \code{\link{draw.boxes}}, \code{\link{draw.steps}}, \code{\link{draw.hist}}, \code{\link{draw.pileup}}, \code{\link{draw.seq}}
}

