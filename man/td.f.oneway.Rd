\name{td.f.oneway}
\alias{td.f.oneway}
\title{
One way F Test
}
\description{
Function to produce One Way F Test.
}
\usage{
td.f.oneway(tdf, col1, col2, thresh = 0.05)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{col1}{
    first column name to test.
}
  \item{col2}{
    second column name to test.
}
  \item{thresh}{
    threshold value.  Default is 0.05.
}
}
\details{
Fun the one way F Test on the td data frame and this column and return a data
frame result of one row.
}
\value{
data frame of one row with F Test results.
}
\note{
Several statistical functions like this one use the Teradata Warehouse Miner
statistical tables and they need to be installed to use this function.  You can
also set up tdMetadataDB once you know where they are installed otherwise they
will be searched for.
}
\examples{

\dontrun{
td.f.oneway(tdf, "income", "age")
}

}
