\name{summary.td.data.frame}
\alias{summary.td.data.frame}
\title{
Summary of Teradata Data Frame
}
\description{
Function which returns from a td data frame information just like R summary on
a data frame.
}
\usage{
\method{summary}{td.data.frame}(tdf, maxsum = 7, digits = max(3, getOption("digits") - 3), ...)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{maxsum}{
    max amount of values for non numeric columns.
}
  \item{digits}{
    digit precision.
}
  \item{\dots}{
    dots to help conform to generic function
}
}
\details{
Function queries the Teradata table and builds up summary results for all the
columns associated with the td data frame.  Similar to summary on a data frame
object.
}
\value{
summary values for all columns.
}
\examples{

\dontrun{
summary(tdf)  #min, max, mean, etc. on all columns of td data frame
}

}
