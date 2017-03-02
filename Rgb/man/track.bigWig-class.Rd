\name{track.bigWig-class}
\Rdversion{1.1}
\docType{class}
\alias{track.bigWig-class}

\title{Class \code{"track.bigWig"}}
\description{
  \code{"track.bigWig"} is a drawing wraper for Big Wiggle files (UCSC Genome Browser).
  
  Notice the data are not stored directly in the object, but stay in the original file, thus exported \code{track.bigWig} objects may be broken (the \code{check} method can confirm this).

  Objects are produced by the \code{\link{track.bigWig}} constructor.
}
\author{
  Sylvain Mareschal
}
\references{
  \url{https://genome.ucsc.edu/goldenpath/help/bigWig.html}
}
\seealso{
  \code{\link{bigWigInstall}}, \code{\link{track.table}}, \code{\link{sliceable-class}}, \code{\link{drawable-class}}
}

\keyword{classes}
\section{Extends}{
  Class \code{\linkS4class{sliceable}}, directly.\cr
  Class \code{\linkS4class{drawable}}, by class \code{\linkS4class{sliceable}}, distance 2.

  All reference classes extend and inherit methods from \code{\linkS4class{envRefClass}}.
}
\section{Fields}{
  \describe{
    binary = "character",
    file = "character",
    organism = "character",
    assembly = "character"
    \item{\code{assembly}:}{Single \code{character} value, the assembly version for the coordinates stored in the object. Must have length 1, should not be \code{NA}.}
    \item{\code{binary}:}{Single \code{character} value, the full path to the bigWigToBedGraph program (see \code{\link{bigWigInstall}}).}
    \item{\code{file}:}{Single \code{character} value, the full path to the bigWig file in use.}
    \item{\code{organism}:}{Single \code{character} value, the name of the organism whose data is stored in the object. Must have length 1, should not be \code{NA}.}
  }
  The following fields are inherited (from the corresponding class):
  \itemize{
    \item name (\linkS4class{drawable})
    \item parameters (\linkS4class{drawable})
  }
}
\section{Methods}{
  The following methods are inherited (from the corresponding class):
  \itemize{
    \item callParams (\linkS4class{drawable})
    \item callSuper (\linkS4class{envRefClass})
    \item check (\linkS4class{drawable}, overloaded)
    \item chromosomes (\linkS4class{drawable})
    \item copy (\linkS4class{envRefClass})
    \item defaultParams (\linkS4class{sliceable}, overloaded)
    \item draw (\linkS4class{sliceable})
    \item export (\linkS4class{envRefClass})
    \item field (\linkS4class{envRefClass})
    \item fix.param (\linkS4class{drawable})
    \item getChromEnd (\linkS4class{sliceable})
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
