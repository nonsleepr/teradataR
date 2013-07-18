\name{median.td.data.frame}
\alias{median.td.data.frame}
\title{
Median Value
}
\description{
Compute median value.
}
\usage{
\method{median}{td.data.frame}(x)
}
\arguments{
  \item{x}{
    td data frame.
}
}
\details{
Function runs against Teradata and finds the median value of a particular column.
}
\value{
returns numeric median value
}
\examples{

\dontrun{
median(tdf["age"])
}

}
