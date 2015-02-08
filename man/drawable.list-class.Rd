\name{drawable.list-class}
\Rdversion{1.1}
\docType{class}
\alias{drawable.list-class}

\title{Class \code{"drawable.list"}}
\description{
  The purpose of this class is to store and manage a collection of \code{\linkS4class{drawable}} objects. These collections are to be used by \code{\link{tk.browse}} and \code{\link{browsePlot}} as input.
  
  Objects can be created by the \code{\link{drawable.list}} constructor, and edited / created using the \code{\link{tk.tracks}} Tcl-tk interface.
}
\author{
  Sylvain Mareschal
}
\seealso{
  \code{\link{drawable.list}}, \code{\link{drawable-class}}, \code{\link{tk.browse}}, \code{\link{browsePlot}}
}

\keyword{classes}
\section{Extends}{
  All reference classes extend and inherit methods from \code{\linkS4class{envRefClass}}.
}
\section{Fields}{
  \describe{
    \item{\code{classes}:}{Read-only, returns a vector of \code{objects} classes.}
    \item{\code{count}:}{Read-only, returns the length of \code{objects}, as a single integer.}
    \item{\code{files}:}{Character vector, the paths where each drawable object is to be stored.}
    \item{\code{hidden}:}{Logical vector, whether each object is to drawn or hidden in plots.}
    \item{\code{names}:}{Read-only, returns a vector of \code{objects} 'name' fields.}
    \item{\code{objects}:}{List of \code{\linkS4class{drawable}}-inheriting objects.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{add(file, track = , hidden = )}:}{
      Add a track to the list.\cr
      - \strong{file}     : single character value, the path to the file containing the 'drawable' object to add.\cr
      - \strong{track}    : a 'drawable' object to add. If NULL, will be extracted from 'file'.\cr
      - \strong{hidden}   : single logical value, whether the track is to be shown on plots or hidden. This value can be changed later.
    }
    \item{\code{check(warn = )}:}{
      Raises an error if the object is not valid, else returns TRUE
    }
    \item{\code{fix.files(parent = )}:}{
      Edit drawable list using a Tcl-tk GUI\cr
      - \strong{parent}   : tcltk parent frame for inclusion, or NULL.
    }
    \item{\code{fix.param(selection = , parent = )}:}{
      Edit drawing parameters using a Tcl-tk GUI\cr
      - \strong{selection}   : single integer value, the position of the track selected in the list.\cr
      - \strong{parent}      : tcltk parent frame for inclusion, or NULL.
    }
    \item{\code{get(index, what = )}:}{
      Returns a single 'what' from the series\cr
      - \strong{index}   : single numeric value, the position of the track to get.\cr
      - \strong{what}    : single character value, the field to be exracted.
    }
    \item{\code{getByClasses(classes, what = )}:}{
      Returns a subset of 'what' from the series, querying by class inheritance\cr
      - \strong{classes}   : character vector, the class names of the objects to get (inheriting classes are picked too).\cr
      - \strong{what}      : single character value, the field to be exracted.
    }
    \item{\code{getByNames(names, what = )}:}{
      Returns a subset of 'what' from the series, querying by track name\cr
      - \strong{names}   : character vector, the names of the objects to get.\cr
      - \strong{what}    : single character value, the field to be exracted.
    }
    \item{\code{getByPositions(positions, what = )}:}{
      Returns a subset of 'what' from the series, querying by position\cr
      - \strong{positions}   : integer vector, the positions of the objects to get.\cr
      - \strong{what}        : single character value, the field to be exracted.
    }
    \item{\code{initialize(files = , objects = , hidden = , ...)}:}{}
    \item{\code{moveDown(toMove)}:}{
      Increases the position of a track, switching position with the next one\cr
      - \strong{toMove}   : single numeric value, the position of the track to move.
    }
    \item{\code{moveUp(toMove)}:}{
      Decreases the position of a track, switching position with the previous one\cr
      - \strong{toMove}   : single numeric value, the position of the track to move.
    }
    \item{\code{remove(toRemove)}:}{
      Remove one or many tracks from the list\cr
      - \strong{toRemove}   : numeric vector, the positions of the tracks to remove.
    }
  }
  The following methods are inherited (from the corresponding class):
  \itemize{
    \item callSuper (\linkS4class{envRefClass})
    \item copy (\linkS4class{envRefClass})
    \item export (\linkS4class{envRefClass})
    \item field (\linkS4class{envRefClass})
    \item getClass (\linkS4class{envRefClass})
    \item getRefClass (\linkS4class{envRefClass})
    \item import (\linkS4class{envRefClass})
    \item initFields (\linkS4class{envRefClass})
    \item show (\linkS4class{envRefClass}, overloaded)
    \item trace (\linkS4class{envRefClass})
    \item untrace (\linkS4class{envRefClass})
    \item usingMethods (\linkS4class{envRefClass})
  }
}
