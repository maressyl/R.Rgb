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
    labelAdj = "center", labelOverflow = TRUE, labelFamily = "sans",
    labelColor = "#000000", fillColor = "#BBBBBB", border = "#666666", cex.lab = 1,
    spacing = 0.1, bty = "o", fg = "#000000", ...)
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
  \item{labelColor}{
    The color to use for box labels (as a name, an integer or an hexadecimal character description). It can alternatively be a function without argument, which returns a vector of as many colors as \code{boxes} has rows. It can make direct variable call to any argument described on this page (including custom arguments passed via "...").
}
  \item{fillColor}{
    The color to fill boxes with (as a name, an integer or an hexadecimal character description). It can alternatively be a function without argument, which returns a vector of as many colors as \code{slice} has rows. It can make direct variable call to any argument described on this page (including custom arguments passed via "...").
}
  \item{border}{
    The color to use for box borders (as a name, an integer or an hexadecimal character description). It can alternatively be a function without argument, which returns a vector of as many colors as \code{slice} has rows. It can make direct variable call to any argument described on this page (including custom arguments passed via "..."). Special values \code{NA} and \code{"fillColor"} can also be used to disable borders or use \code{fillColor} respectively.
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
  \item{fg}{
    Single character value, defining the color of the foreground (axes, labels...) as an english name or a hexadecimal code. Similar to \code{par}'s argument but not relying on it.
}
  \item{\dots}{
    Further arguments to be passed to \code{\link{draw.bg}}.
}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{yline}}
  
  \code{\link{draw.bg}}, \code{\link{draw.boxes}}, \code{\link{draw.density}}, \code{\link{draw.hist}}, \code{\link{draw.pileup}}, \code{\link{draw.points}}, \code{\link{draw.seq}}
}

