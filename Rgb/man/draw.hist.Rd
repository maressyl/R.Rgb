\name{draw.hist}
\alias{draw.hist}
\title{
  Histogram plot of a track
}

\description{
  This function draws a slice of a track content, with a distinct vertical bar for each track element.
}

\usage{
  draw.hist(slice, start, end, column = "value", colorVal = "#666666",
    colorFun = function() NULL, border = "#666666", cex.lab = 1, origin = 0,
    bty = "o", ...)
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
  \item{border}{
    The color to fill boxe borders with (as a name, an integer or an hexadecimal character description).
}
  \item{cex.lab}{
    The relative character size of x and y axis labels (default: 1). See \code{par}.
}
  \item{origin}{
    Single numeric value, the Y value of the horizontal side common to all boxes.
}
  \item{bty}{
    A character string which determined the type of box which is drawn about plots. If bty is one of "o" (the default), "l", "7", "c", "u", or "]" the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box. See \code{par}.
}
  \item{\dots}{
    Further arguments to be passed to \code{\link{draw.bg}}.
}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{draw.bg}}, \code{\link{draw.boxes}}, \code{\link{draw.steps}}, \code{\link{draw.points}}, \code{\link{draw.pileup}}, \code{\link{draw.seq}}
}

