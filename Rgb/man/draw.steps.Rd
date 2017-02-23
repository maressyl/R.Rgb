\name{draw.steps}
\alias{draw.steps}
\title{
  Stairway plot of a track
}

\description{
  This function draws each element sliced from a track as a separate podium, defined by several start and end genomic coordinates. This representation may prove useful to represent results of Minimal Common Regions from algorithms such as SRA or GISTIC (see the 'cghRA' package).
}

\usage{
  draw.steps(slice, start, end, startColumns = "start", endColumns = "end",
    maxDepth = 100, label = TRUE, labelStrand = FALSE, labelCex = 1, labelSrt = 0,
    labelAdj = "center", labelOverflow = TRUE, labelFamily = "sans", colorVal = "#BBBBBB",
    colorFun = function() NULL, border = "#666666", cex.lab = 1, spacing = 0.1, bty = "o",
    ...)
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
  \item{startColumns}{
    Character vector naming the columns in \code{slice} to be used as step starts, from the widest to the narrowest.
}
  \item{endColumns}{
    Character vector naming the columns in \code{slice} to be used as step ends, from the widest to the narrowest.
}
  \item{maxDepth}{
    Single integer value, the maximum amount of box heights allowed on the plot to avoid overlaps (if exhausted an error message will be ploted, turning \code{label} to \code{FALSE} might help).
}
  \item{label}{
    Single logical value, whether to print labels on boxes or not.
}
  \item{labelStrand}{
    Single logical value, whether to add the strand at the end of labels or not.
}
  \item{labelCex}{
    Single numeric value, character expansion factor for labels.
}
  \item{labelSrt}{
    Single numeric value, string rotation angle for labels.
}
  \item{labelAdj}{
    'left', 'right' or 'center', the horizontal adjustement of the labels on the boxes.
}
  \item{labelOverflow}{
    Single logical value, whether to write labels on boxes too narrow to host them or not.
}
  \item{labelFamily}{
    Single character value, the font family to use for labels ('serif', 'sans', 'mono' or 'Hershey'). 'serif' and 'sans' are not monospaced fonts, so label box sizes and collision handling might not work as expected with them.
}
  \item{colorVal}{
    The color to fill boxes with (as a name, an integer or an hexadecimal character description).
}
  \item{colorFun}{
    A function with no arguments, which returns a vector of as many colors than the \code{slice} has rows. It can make use of any argument described on this page (including custom arguments passed via "..."), as its enclosing environment is redefined to the calling one. \code{colorVal} must be \code{NA} for the function to be used.
}
  \item{border}{
    The color to fill box borders with (as a name, an integer or an hexadecimal character description).
}
  \item{cex.lab}{
    The relative character size of x and y axis labels (default: 1). See \code{par}.
}
  \item{spacing}{
    Single numeric value, the vertical spacing between boxes, in proportion of the box height.
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
  \code{\link{draw.bg}}, \code{\link{draw.boxes}}, \code{\link{draw.hist}}, \code{\link{draw.points}}, \code{\link{draw.pileup}}, \code{\link{draw.seq}}
  
  \code{\link{yline}}
}

