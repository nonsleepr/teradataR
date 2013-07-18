\name{td.binomialsign}
\alias{td.binomialsign}
\title{
Binomial Sign Test
}
\description{
Function to run a binomial sign test on a Teradata column.
}
\usage{
td.binomialsign(tdf, column, thresh = 0.05)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{column}{
    column name.
}
  \item{thresh}{
    threshold value.  Defaults to 0.05.
}
}
\details{
Function that runs the binomial sign test on a particular column and returns its
results.
}
\value{
data frame of one row of binomial sign results.
}
\note{
Several statistical functions like this one use the Teradata Warehouse Miner
statistical tables and they need to be installed to use this function.  You can
also set up tdMetadataDB once you know where they are installed otherwise they
will be searched for.
}

\examples{

\dontrun{
td.binomialsign(tdf, "age")
}

}
