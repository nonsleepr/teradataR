\name{td.shapiro.wilk}
\alias{td.shapiro.wilk}
\title{
Shapiro Wilk
}
\description{
Function to run Shapiro Wilk test against td data frame and column.
}
\usage{
td.shapiro.wilk(tdf, col, thresh = 0.05)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{col}{
    column name.
}
  \item{thresh}{
    threshold value.  Default is 0.05.
}
}
\details{
Function returns test value for Shapiro Wilk on td data frame and column.
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
td.shapiro.wilk(tdf,"age")
}
}
