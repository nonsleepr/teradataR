\name{td.smirnov}
\alias{td.smirnov}
\title{
Smirnov Test
}
\description{
Function to run Smirnov test on td data frame and columns.
}
\usage{
td.smirnov(tdf, col1, col2, thresh = 0.05)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{col1}{
    first column of test.
}
  \item{col2}{
    second column of test.
}
  \item{thresh}{
    threshold value.  Defaults to 0.05.
}
}
\details{
Run Smirnov test and return one row for test.  Test is run on td data frame
and two input column names.
}
\value{
data frame of one row with test results.
}
\note{
Several statistical functions like this one use the Teradata Warehouse Miner
statistical tables and they need to be installed to use this function.  You can
also set up tdMetadataDB once you know where they are installed otherwise they
will be searched for.
}
\examples{

\dontrun{
td.smirnov(tdf,"income","age")
}

}
