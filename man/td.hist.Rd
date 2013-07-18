\name{td.hist}
\alias{td.hist}
\title{
Histograms
}
\description{
Generate a histogram of a column
}
\usage{
td.hist(tdf, col, breaks = 10, plot = TRUE, ...)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{col}{
    column name.
}
  \item{breaks}{
    groups to break column into.
}
  \item{plot}{
    Whether to plot results or not.  Default is TRUE.
}
  \item{\dots}{
    Allow other commands to be sent to plot.
}
}
\details{
Function builds a histogram object from a Teradata table to represent that
column.  It can then plot the result or use as a histogram object.
}
\value{
A histogram object.
}
\examples{

\dontrun{
td.hist(tdf[3])
td.hist(tdf["age"])
td.hist(tdf, "age")
}

}
