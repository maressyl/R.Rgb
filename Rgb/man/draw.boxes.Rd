\name{draw.boxes}
\alias{draw.boxes}
\title{
  Boxes plot of a track
}

\description{
  This function draws a slice of a track content, with a distinct box for each track element.
}

\usage{
  draw.boxes(slice, start, end, maxElements = 50, maxDepth = 100, label = TRUE,
    labelStrand = FALSE, labelCex = 1, labelSrt = 0, labelAdj = "center",
    labelOverflow = TRUE, labelFamily = "sans", colorVal = "#BBBBBB",
    colorFun = function() NULL, border = "#666666", cex.lab = 1, spacing = 0.2,
    bty = "o", groupBy = NA, groupPosition = NA, groupSize = NA, groupLwd = 1, ...)
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
  \item{maxElements}{
    Single integer value, the maximum amount of boxes on the plot (if exhausted, only the amount of elements will be ploted).
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
  \item{groupBy}{
    Single character value, the name of a \code{slice} column to use for feature grouping. Grouped features are drawn on the same line, and joined by an horizontal segment. Use \code{NA} to disable feature grouping.
}
  \item{groupPosition}{
    Single character value, the name of an integer \code{slice} column with the position of the feature in the group (starting at 1). If \code{groupBy} is provided with \code{groupPosition} and \code{groupSize}, groups containing out-of-range elements will be extended to the screen boundary. Use \code{NA} if feature grouping is disabled, or if awareness of out-of-range features is not critical.
}
  \item{groupSize}{
    Single character value, the name of an integer \code{slice} column with the total amount of features in the group. See \code{groupPosition}.
}
  \item{groupLwd}{
    Single numeric value, the width of the line drawn to group elements.
}
  \item{\dots}{
    Further arguments to be passed to \code{\link{draw.bg}}.
}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{draw.bg}}, \code{\link{draw.steps}}, \code{\link{draw.hist}}, \code{\link{draw.points}}, \code{\link{draw.pileup}}, \code{\link{draw.seq}}
  
  \code{\link{yline}}
}

