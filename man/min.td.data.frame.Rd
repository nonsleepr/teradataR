\name{min.td.data.frame}
\alias{min.td.data.frame}
\title{
Minima
}
\description{
Find the minimum value in the td data frame
}
\usage{
\method{min}{td.data.frame}(tdf, ...)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{\dots}{
    conform to generic min
}
}
\details{
Access database and return the minimum value amoung the current columns in the td data frame.
}
\value{
Returns the minimum value found as a numeric
}
\note{
The td data frame must consist of only numeric variables
}
\examples{

\dontrun{

tdf <- td.data.frame("customer")
min(tdf)
#Returns min value of columns in customer

}
}
