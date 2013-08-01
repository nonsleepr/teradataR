\name{hist.td.data.frame}
\alias{hist.td.data.frame}
\title{
Histograms
}
\description{
Function to describe a td data frame via plots.
}
\usage{
\method{hist}{td.data.frame}(x, breaks = 10, \dots)
}
\arguments{
  \item{x}{
    td data frame.
}
  \item{breaks}{
    number of groups to break each column up into.
}
  \item{\dots}{
    forward information to plots.
}
}
\details{
This function builds a histogram for each numeric column in the td data frame.
A frequency graph is produced for non numeric columns.  The max amount of graphs
is limited to 16 so if you have more columns you can run the command on different
ranges to see all the plots.
}
\value{
Visual graph of the histogram plots.
}
\examples{

\dontrun{
hist(tdf) #tdf is a td data frame
}

}
