\name{singlePlot}
\alias{singlePlot}
\title{
  Whole genome plot of a single track content
}

\description{
  This function plots all chromosomes on a single plot, with boxes representing the track content.
}

\usage{
  singlePlot(track, bandTrack, columns = 4, exclude = c("X", "Y"))
}

\arguments{
  \item{track}{
    A \code{\linkS4class{track.table}} inheriting object, whose elements will be plotted. If \code{height} and \code{colors} column exist, they will be used to shape the boxes plotted.
}
  \item{bandTrack}{
    A cytoband track, as returned by \code{\link{track.bands.UCSC}}.
}
  \item{columns}{
    Single integer value, column count in the chromosome layout.
}
  \item{exclude}{
    Character vector, the names of the chromosomes to not plot.
}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{track.table}}
}

