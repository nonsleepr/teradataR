\name{max.td.data.frame}
\alias{max.td.data.frame}
\title{
Maxima
}
\description{
Find the maximum value in the td data frame
}
\usage{
\method{max}{td.data.frame}(tdf, \dots)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{\dots}{
    conform to generic max
}
}
\details{
Access database and return the maximum value amoung the current columns in the td data frame.
}
\value{
Returns the maximum value found as a numeric
}
\note{
The td data frame must consist of only numeric variables
}

\examples{

\dontrun{

tdf <- td.data.frame("customer")
max(tdf)
#Returns max value of columns in customer

}
}
