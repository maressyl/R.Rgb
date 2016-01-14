\name{track.bam-class}
\Rdversion{1.1}
\docType{class}
\alias{track.bam-class}

\title{Class \code{"track.bam"}}
\description{
  \code{"track.bam"} is a drawing wraper for Binary Alignment Map files (SAMtools).
  
  Notice the data are not stored directly in the object, but stay in the original BAM file, thus exported \code{track.bam} objects may be broken (the \code{check} method can confirm this).

  Objects are produced by the \code{\link{track.bam}} constructor.
}
\author{
  Sylvain Mareschal
}
\references{
  \url{http://samtools.github.io/hts-specs/SAMv1.pdf}
}
\seealso{
  \code{\link{track.table}}, \code{\link{sliceable-class}}, \code{\link{drawable-class}}
}

\keyword{classes}
\section{Extends}{
  Class \code{\linkS4class{sliceable}}, directly.\cr
  Class \code{\linkS4class{drawable}}, by class \code{\linkS4class{sliceable}}, distance 2.

  All reference classes extend and inherit methods from \code{\linkS4class{envRefClass}}.
}
\section{Fields}{
  \describe{
    \item{\code{addChr}:}{Single \code{logical} value, whether to automatically add 'chr' ahead chromosome names when querying or not..}
    \item{\code{assembly}:}{Single \code{character} value, the assembly version for the coordinates stored in the object. Must have length 1, should not be \code{NA}.}
    \item{\code{baiPath}:}{Single \code{character} value, the full path to the BAI index file in use.}
    \item{\code{bamPath}:}{Single \code{character} value, the full path to the BAM file in use.}
    \item{\code{header}:}{A \code{data.frame} describing the @SQ elements of the BAM header (one per row).}
    \item{\code{index}:}{The parsed content of the BAI index, as a unamed \code{list} with one element by reference sequence, itself a \code{list} with 'bins' and 'intervals' elements. 'bins' is a named \code{list} of two-column matrices ('start' and 'end'), giving virtual BGZF coordinates of the described bin (as \code{double}). 'intervals' is a \code{double} vector of virtual BGZF coordinates, used for linear filtering (see SAM specification for further details).}
    \item{\code{organism}:}{Single \code{character} value, the name of the organism whose data is stored in the object. Must have length 1, should not be \code{NA}.}
  }
  The following fields are inherited (from the corresponding class):
  \itemize{
    \item name (\linkS4class{drawable})
    \item parameters (\linkS4class{drawable})
  }
}
\section{Methods}{
  \describe{
    \item{\code{crawl(chrom, start, end, addChr = , verbosity = , ..., init, loop, final)}:}{
      Apply a custom processing to reads in a genomic window.\cr
      - \strong{chrom}       : single integer, numeric or character value, the chromosomal location. NA is not handled.\cr
      - \strong{start}       : single integer or numeric value, inferior boundary of the window. NA is not handled.\cr
      - \strong{end}         : single integer or numeric value, superior boundary of the window. NA is not handled.\cr
      - \strong{addChr}      : single logical value, whether to systematically add 'chr' in front of the 'chrom' value or not.\cr
      - \strong{verbosity}   : single integer value, the level of verbosity during processing (0, 1 or 2).\cr
      - \strong{...}         : arguments to be passed to 'init', 'loop' or 'final'.\cr
      - \strong{init}        : an expression to be evaluated before looping on reads.\cr
      - \strong{loop}        : a function with taking at least 'read' and '...' as arguments, modifying 'output' in the parent environment.\cr
      - \strong{final}       : an expression to be evaluated after looping on reads.
    }
    \item{\code{depth(..., qBase = , qMap = )}:}{
      Counts covering bases for each genomic position, similarly to SAMtools' depth.\cr
      - \strong{...}     : argumets to be passed to the crawl() method.\cr
      - \strong{qBase}   : single integer value, minimal base quality for a base to be counted.\cr
      - \strong{qMap}    : single integer value, minimal mapping quality for a base to be counted.
    }
    \item{\code{extract(...)}:}{
      Extract reads as a list, similarly to SAMtools' view.\cr
      - \strong{...}     : argumets to be passed to the crawl() method.
    }
    \item{\code{pileup(..., qBase = , qMap = )}:}{
      Counts each nucleotide type for each genomic position, similarly to SAMtools' mpileup.\cr
      - \strong{...}     : argumets to be passed to the crawl() method.\cr
      - \strong{qBase}   : single integer value, minimal base quality for a base to be counted.\cr
      - \strong{qMap}    : single integer value, minimal mapping quality for a base to be counted.
    }
  }
  The following methods are inherited (from the corresponding class):
  \itemize{
    \item callParams (\linkS4class{drawable})
    \item callSuper (\linkS4class{envRefClass})
    \item check (\linkS4class{drawable})
    \item chromosomes (\linkS4class{drawable})
    \item copy (\linkS4class{envRefClass})
    \item defaultParams (\linkS4class{sliceable}, overloaded)
    \item draw (\linkS4class{sliceable})
    \item export (\linkS4class{envRefClass})
    \item field (\linkS4class{envRefClass})
    \item fix.param (\linkS4class{drawable})
    \item getChromEnd (\linkS4class{sliceable}, overloaded)
    \item getClass (\linkS4class{envRefClass})
    \item getName (\linkS4class{drawable})
    \item getParam (\linkS4class{drawable})
    \item getRefClass (\linkS4class{envRefClass})
    \item import (\linkS4class{envRefClass})
    \item initFields (\linkS4class{envRefClass})
    \item initialize (\linkS4class{drawable}, overloaded)
    \item setName (\linkS4class{drawable})
    \item setParam (\linkS4class{drawable})
    \item show (\linkS4class{sliceable}, overloaded)
    \item slice (\linkS4class{sliceable}, overloaded)
    \item trace (\linkS4class{envRefClass})
    \item untrace (\linkS4class{envRefClass})
    \item usingMethods (\linkS4class{envRefClass})
  }
}
