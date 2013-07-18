\name{as.data.frame.td.data.frame}
\alias{as.data.frame.td.data.frame}
\title{
Convert td data frame to a data frame
}
\description{
Coerce a td data frame object into a data frame.  This will access the
Teradata object referenced by the td data frame and select rows from
that object and bring it over as a data frame.
}
\usage{
\method{as.data.frame}{td.data.frame}(x, size = 100)
}
\arguments{
  \item{x}{td data frame.}
  \item{size}{max amount of rows to return from database.}
}
\value{
data frame
}

\examples{

\dontrun{
tdf <- td.data.frame("customer")
df <- as.data.frame(tdf)
# Brings 'size' rows of data from Teradata to a data frame
}

}
