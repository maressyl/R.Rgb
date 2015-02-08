\name{tk.browse}
\alias{tk.browse}
\alias{browsePlot}
\title{
  R Genome browser
}

\description{
  The \code{browsePlot} function produces an usual R plot from a \code{\linkS4class{drawable}} inheriting object list, at specific coordinates in the genome.
  
  The \code{tk.browse} function summons a TCL-TK interface to navigate through the whole genome, relying on \code{browsePlot} for the plotting.
  
  The former may be called directly to automatically export views from the genome browser, the latter is more suited to an interactive browsing with frequent coordinate jumps.
}

\usage{
  tk.browse(drawables, hscale = NA, vscale = NA, blocking = FALSE,
    scaleFactor = NA, updateLimit = 0.4)
  browsePlot(drawables, chrom = NA, start = NA, end = NA,
    customLayout = FALSE, xaxt = "s", xaxm = 1.5, ...)
}

\arguments{
  \item{drawables}{
    A \code{\linkS4class{drawable.list}} object containing the tracks to plot, as returned by \code{\link{drawable.list}} or \code{\link{tk.tracks}}.
}
  \item{hscale}{
    Single numeric value, to be passed to \code{tkrplot}.
}
  \item{vscale}{
    Single numeric value, to be passed to \code{tkrplot}.
}
  \item{blocking}{
    Single logical value, whether to wait for the interface window to be closed before unfreezing the R console. The \code{FALSE} default let you use R and the interface in parallel (see the 'Typing R commands while browsing' section below), the code{TRUE} value is used essentially in the stand alone version.
}
  \item{scaleFactor}{
    Single numeric value, scaling factor to convert pixel sizes in \code{tkrplot} scale units. \code{NA} will use the OS dependant default values.
}
  \item{updateLimit}{
    Single numeric value, minimal time (in seconds) between two image updates when move or zoom key are continuously pressed. This is used to limit zoom and move speed on fast computers.
}
  \item{chrom}{
    Single character value, the chromosome to plot.
}
  \item{start}{
    Single integer value, the left boundary of the window to plot. \code{NA} will use 0.
}
  \item{end}{
    Single integer value, the right boundary of the window to plot. \code{NA} will use the maximal drawable$getChromEnd() value throughout \code{trackList}.
}
  \item{customLayout}{
    Single logical value, whether to organize the various plot or not. If \code{FALSE}, \code{\link{layout}} will be called and you will not be able to add custom plots. If \code{TRUE}, it is up to the user to define its \code{\link{layout}} call to handle the track plots that will be issued by the function.
}
  \item{xaxt}{
    X axis showing (see \code{par}). Only applied to the last track, other ones never show their X axis ("n").
}
  \item{xaxm}{
    Minimal bottom margin for the last track (see \code{par}). It assures enough margin is reserved for X axis showing (see \code{xaxt}).
}
  \item{\dots}{
    Further arguments are passed through to the \code{draw} method of each track in \code{trackList}.
}
}

\section{Interactive browsing}{
  \subsection{Jump to specific location}{The left upper panel can be used to jump to specific coordinates, defined by a chromosome name, a starting position and an ending position (as floating point numerics in millions of base pairs.}
  \subsection{Move along a chromosome}{The left and right arrow keys may be used to shift the window to the corresponding side.}
  \subsection{Generic zoom}{The up and down arrow keys, as well as the vertical mouse wheel, may be used to zoom in or out on the current location.}
  \subsection{Localized zoom in}{A zoom can also be achieved with a mouse drag on the region to investigate : maintain a left click on the position to use as the new left boundary, and release the click at the position of the new right boundary.}
  \subsection{Resize plot area}{The plot area size is defined by \code{hscale} and \code{vscale}. During interactive browsing, resize the browser window and use the "r" key to adjust the plot area to the window size.}
}

\section{Typing R commands while browsing}{
  \code{tk.browse} returns the \code{\linkS4class{drawable.list}} objects it uses to store currently browsed data. As it is a \link[=setRefClass]{reference class} object, the same memory location is shared by \code{tk.browse} and the returned object, so updates (like track addition or edition) made by \code{tk.browse} will impact the object in the R Command Line Interface (CLI), and updates made via R commands will impact the current \code{tk.browse} session.
  
  Some sub-interfaces (like information pop-ups and track selection panels) may freeze the R command prompt while opened, make sure to have only the \code{tk.browse} main window opened when typing R commands.
  
  Notice some operating systems (including Windows) restrain users to type R commands while a tcl-tk window is opened, or seems to be instable while doing so. Setting \code{blocking} to \code{TRUE} will enforce this behavior, keeping users from typing commands while \code{tk.browse} is running.
}

\value{
  \code{tk.browse} invisibly returns the \code{drawables} argument (See the 'Typing R commands while browsing' section above).
  
  \code{browsePlot} invisibly returns the \code{par} function output for the last track plot, as it is used by \code{tk.browse}.
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\linkS4class{drawable}}, \code{\linkS4class{track.table}}
}
