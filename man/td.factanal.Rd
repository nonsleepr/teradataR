\name{td.factanal}
\alias{td.factanal}
\title{
Factor Analysis
}
\description{
Run covariance matrix and send results into R factanal function.
}
\usage{
td.factanal(tdf, ...)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{\dots}{
    conform to generic function and pass values like factors.
}
}
\details{
Build a covariance matrix in Teradata and call factanal with matrix.  ... is
specified so you and send how many factors you'd like to the factanal function.
}
\value{
factor analysis report.
}
\examples{

\dontrun{
td.factanal(tdf[1:4])  #limit to certain columns in td data frame
}

}
