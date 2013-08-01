\name{td.tapply}
\alias{td.tapply}
\title{
Apply a function over a database column
}
\description{
Apply a function to each column value, that is to each (non-empty) group of values given by a unique combination of the levels of certain factors.
}
\usage{
td.tapply(X, INDEX, FUN = NULL, asdf = FALSE, ...)
}
\arguments{
  \item{X}{
    td data frame of column to apply function to.
}
  \item{INDEX}{
    td data frame of the list of columns, each in same table as X.
}
  \item{FUN}{
    One of the following functions to be applied (sum, min, max, mean, td.values, and td.stats).
}
  \item{asdf}{
    If TRUE, the return value is left as a data frame object.  Otherwise default is as an array.
}
  \item{\dots}{
    forward information.
}
}
\details{
}
\value{
Array dimensioned by the INDEX containing the result of applying FUN to the data.  If asdf = TRUE
the result will instead be a data frame of the results.
}
\examples{

\dontrun{
td.tapply(tdf["income"], tdf["gender"], td.stats)
}

}
