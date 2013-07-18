\name{td.t.paired}
\alias{td.t.paired}
\title{
T Test Paired
}
\description{
Function to run a paired T Test
}
\usage{
td.t.paired(tdf, col1, col2, thresh = 0.05)
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
    threshold.  Default value is 0.05
}
}
\details{
The test is whether paired values have mean differences which are not significantly 
different from zero. It assumes differences are identically distributed normal 
random variables, and that they are independent.}
\value{
Single row data frame with the T Test results
}
\examples{

\dontrun{
td.t.paired(tdf, "income", "age")
}

}
