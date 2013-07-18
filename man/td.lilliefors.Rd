\name{td.lilliefors}
\alias{td.lilliefors}
\title{
Lilliefors Test
}
\description{
Function to use lilliefors test on table and column.
}
\usage{
td.lilliefors(tdf, col, thresh = 0.05)
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
Use statistical tables to generate the lilliefors test statistic for this
td data frame and column.
}
\value{
data frame with one row of results.
}
\note{
Several statistical functions like this one use the Teradata Warehouse Miner
statistical tables and they need to be installed to use this function.  You can
also set up tdMetadataDB once you know where they are installed otherwise they
will be searched for.
}
\examples{

\dontrun{
td.lilliefors(tdf,"age")
}

}
