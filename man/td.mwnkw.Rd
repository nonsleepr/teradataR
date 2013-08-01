\name{td.mwnkw}
\alias{td.mwnkw}
\title{
Mann-Whitney/Kruskal Wallis Test
}
\description{
Function to test column in a td data frame with MW/KW.
}
\usage{
td.mwnkw(tdf, col1, col2, thresh = 0.05)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{col1}{
    first column to test.
}
  \item{col2}{
    second column to test.
}
  \item{thresh}{
    threshold value.  Defaults to 0.05.
}
}
\details{
Function to find the MW/KW test value of two columns in a td data frame.
}
\value{
data frame with MW/KW test results.
}
\note{
Several statistical functions like this one use the Teradata Warehouse Miner
statistical tables and they need to be installed to use this function.  You can
also set up tdMetadataDB once you know where they are installed otherwise they
will be searched for.
}
\examples{

\dontrun{
td.mwnkw(tdf,"income","age")
}

}
