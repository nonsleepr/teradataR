\name{sum.td.data.frame}
\alias{sum.td.data.frame}
\title{
Sum of column
}
\description{
sum returns the sum of all the values present in a column.
}
\usage{
\method{sum}{td.data.frame}(tdf, \dots)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{\dots}{
    conform to generic sum.
}
}
\details{
This is an override of the generic sum function.  Acces the Teradata system and
return the sum of a particular column or columns.
}
\value{
A numeric value of the sum.
}
\examples{

\dontrun{
sum(tdf[1:5])  #total sum of 5 columns from td data frame
}

}
