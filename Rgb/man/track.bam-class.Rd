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
    \item{\code{compression}:}{Single \code{numeric} value, an estimation of the BAM file compression ratio.}
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
    \item{\code{coverage(chrom, start = , end = , tracks = , binLevel = , rawSize = )}:}{
      Fast estimation of depth coverage in a genomic window, from indexing data. Values are normalized into [0:1] over the genomic window.\cr
      - \strong{chrom}      : single integer, numeric or character value, the chromosomal location.\cr
      - \strong{start}      : single integer or numeric value, inferior boundary of the window. If NA, the whole chromosome is considered.\cr
      - \strong{end}        : single integer or numeric value, superior boundary of the window. If NA, the whole chromosome is considered.\cr
      - \strong{tracks}     : single logical value, whether to return a data.frame or a track.table.\cr
      - \strong{binLevel}   : single integer value, the higher bin order to allow\cr
                     0 = 537Mb, 1 = 67Mb, 2 = 8Mb, 3 = 1Mb, 4 = 130kb, 5 = 16kb\cr
                     incrementing this value enhances boundary precision but discards reads located at bin junctions.\cr
      - \strong{rawSize}    : single logical value, whether to output raw size or normalize by the maximum encountered.
    }
    \item{\code{crawl(chrom, start, end, addChr = , maxRange = , verbosity = , ..., init, loop, final)}:}{
      Apply a custom processing to reads in a genomic window.\cr
      - \strong{chrom}       : single integer, numeric or character value, the chromosomal location. NA is not handled.\cr
      - \strong{start}       : single integer or numeric value, inferior boundary of the window. NA is not handled.\cr
      - \strong{end}         : single integer or numeric value, superior boundary of the window. NA is not handled.\cr
      - \strong{addChr}      : single logical value, whether to systematically add 'chr' in front of the 'chrom' value or not.\cr
      - \strong{maxRange}    : single integer value, no extraction will be attempted if end and start are more than this value away (returns NULL).\cr
      - \strong{verbosity}   : single integer value, the level of verbosity during processing (0, 1 or 2).\cr
      - \strong{...}         : arguments to be passed to 'init', 'loop' or 'final'.\cr
      - \strong{init}        : an expression to be evaluated before looping on reads.\cr
      - \strong{loop}        : a function with taking at least 'read' and '...' as arguments, modifying 'output' in the parent environment.\cr
                      The 'earlyBreak' logical can also be modified in the parent environment to stop the looping immediately.\cr
      - \strong{final}       : an expression to be evaluated after looping on reads.
    }
    \item{\code{depth(..., qBase = , qMap = )}:}{
      Counts covering bases for each genomic position, similarly to SAMtools' depth.\cr
      - \strong{...}     : arguments to be passed to the crawl() method.\cr
      - \strong{qBase}   : single integer value, minimal base quality for a base to be counted.\cr
      - \strong{qMap}    : single integer value, minimal mapping quality for a base to be counted.
    }
    \item{\code{extract(...)}:}{
      Extract reads as a list, similarly to SAMtools' view.\cr
      - \strong{...}     : arguments to be passed to the crawl() method.
    }
    \item{\code{getBlocks(limit = , quiet = )}:}{
      Jump from BGZF blocks to blocks, recording compressed (bsize) and uncompressed (isize) block sizes\cr
      - \strong{limit}   : single integer value, the amount of blocks to evaluate (NA for the whole BAM file, may be very time consuming).\cr
      - \strong{quiet}   : single logical value, whether to throw diagnostic messages or not.
    }
    \item{\code{getCompression(sample = )}:}{
      Estimate BGZF block compression level from a sample of blocks\cr
      - \strong{sample}   : single integer value, the amount of blocks to use for estimation (the first block is ignored).
    }
    \item{\code{pileup(..., qBase = , qMap = )}:}{
      Counts each nucleotide type for each genomic position, similarly to SAMtools' mpileup.\cr
      - \strong{...}     : arguments to be passed to the crawl() method.\cr
      - \strong{qBase}   : single integer value, minimal base quality for a base to be counted.\cr
      - \strong{qMap}    : single integer value, minimal mapping quality for a base to be counted.
    }
    \item{\code{summary(chrom = , tracks = , binLevel = , rawSize = )}:}{
      Fast estimation of depth coverage for the whole genome, from indexing data. Values are normalized into [0:1] over the whole genome.\cr
      - \strong{chrom}      : character vector, the names of the chromosome to query. If NA, all chromosomes will be queried.\cr
      - \strong{tracks}     : single logical value, whether to return a data.frame or a track.table.\cr
      - \strong{binLevel}   : single integer value, the higher bin order to allow\cr
                     0 = 537Mb, 1 = 67Mb, 2 = 8Mb, 3 = 1Mb, 4 = 130kb, 5 = 16kb\cr
                     incrementing this value enhances boundary precision but discards reads located at bin junctions\cr
      - \strong{rawSize}    : single logical value, whether to output raw size or normalize by the maximum encountered.
    }
  }
  The following methods are inherited (from the corresponding class):
  \itemize{
    \item callParams (\linkS4class{drawable})
    \item callSuper (\linkS4class{envRefClass})
    \item check (\linkS4class{drawable}, overloaded)
    \item chromosomes (\linkS4class{drawable}, overloaded)
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
