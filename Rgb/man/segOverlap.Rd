\name{segOverlap}
\alias{segOverlap}
\title{
  Merges overlapping segments
}

\description{
  Given a set of segments defined by "chrom", "start", "end" and various data, it merges overlapping or jointive rows. 
}

\usage{
  segOverlap(segTable, fun = list(unique.default, start=min, end=max))
}

\arguments{
  \item{segTable}{
    A \code{data.frame} of segments, with at least "chrom", "start" and "end" columns. Standard behavior (default \code{fun} value) assumes "start" and "end" to be at least \code{numeric}, preferably \code{integer}.
}
  \item{fun}{
    A \code{list} of \code{function}s, defining how to merge values when merging rows. It should contain an unamed element for the default function, and named elements to deal with specific columns.
}
}

\value{
  Returns a \code{data.frame} similar to \code{segTable}.
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{segMerge}}
}

