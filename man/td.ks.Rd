\name{td.ks}
\alias{td.ks}
\title{
Kolmogorov Smirnov Test
}
\description{
Run Kolmogorov Smirnov test against a Teradata table and column.
}
\usage{
td.ks(tdf, col, thresh = 0.05)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{col}{
    column name to test.
}
  \item{thresh}{
    threshold value.  Defaults to 0.05.
}
}
\details{
Find KS statistics from a table and column in Teradata.
}
\value{
data frame with one row of test results.
}
\note{
Several statistical functions like this one use the Teradata Warehouse Miner
statistical tables and they need to be installed to use this function.  You can
also set up tdMetadataDB once you know where they are installed otherwise they
will be searched for.
}
\examples{

\dontrun{
td.ks(tdf,"age")
}

}
