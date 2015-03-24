\name{singlePlot}
\alias{singlePlot}
\title{
  Whole genome plot of a single drawable content
}

\description{
  This function plots a single drawable object on all the chromosomes side by side, with a constant X axis scale.
}

\usage{
  singlePlot(object, bands, columns = 4, bandHeight = lcm(1), exclude = c("X", "Y"),
    object.args = list(), bands.args = list())
}

\arguments{
  \item{object}{
    A \code{\linkS4class{drawable}} inheriting object, whose elements will be drawn.
}
  \item{bands}{
    A \code{\linkS4class{track.bands}} inheriting object, as returned by \code{\link{track.bands.UCSC}}.
}
  \item{columns}{
    Single integer value, column count in the chromosome layout.
}
  \item{bandHeight}{
    Single character or numeric value, the height to be used in \code{layout} for cytoband plots.
}
  \item{exclude}{
    Character vector, the names of the chromosomes to not plot.
}
  \item{object.args}{
    A named list of optional graphical parameters to apply to \code{object} before drawing.
}
  \item{bands.args}{
    A named list of optional graphical parameters to apply to \code{bandTrack} before drawing.
}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\linkS4class{drawable}}
}

