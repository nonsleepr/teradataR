\name{teradataR-package}
\alias{teradataR-package}
\alias{teradataR}
\docType{package}
\title{
Allow access to Teradata via R
}
\description{
This package is designed to allow users of R to interact with a Teradata database.
Users can use many statistical functions directly against the Teradata system
without having to extract the data into memory.
}
\details{
\tabular{ll}{
Package: \tab teradataR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-10-11\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}
}
\author{
Todd Brye

Maintainer: <todd.brye@teradata.com>
}

\keyword{ package }
\examples{

\dontrun{
tdConnect("tdSystem")
tdf <- td.data.frame("transactions")
td.stats(tdf["column"])
}

}
