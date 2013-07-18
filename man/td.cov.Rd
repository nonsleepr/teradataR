\name{td.cov}
\alias{td.cov}
\title{
Covariance Matrix
}
\description{
Create a matrix of the covariance between numeric columns.
}
\usage{
td.cov(tdf)
}
\arguments{
  \item{tdf}{
    td data frame.
}
}
\details{
Build a matrix of all columns in the td data frame.
}
\value{
Return a matrix of numeric covariance values.
}
\examples{

\dontrun{
td.cov(tdf[1:4])  #select only the numeric columns
} 

}
