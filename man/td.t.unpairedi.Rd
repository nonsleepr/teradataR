\name{td.t.unpairedi}
\alias{td.t.unpairedi}
\title{
T Test - Unpaired Indicator
}
\description{
Function to run unpaired indicator T Test 
}
\usage{
td.t.unpairedi(tdf, col1, col2, thresh = 0.05)
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
This function selects the column of interest and a second indicator column which 
determines to which group the first variable belongs.  If the indicator variable 
is negative or zero, it will be assigned to the first group; if it is positive, 
it will be assigned to the second group.
}
\value{
Single row data frame with the T Test results
}
\examples{

\dontrun{
td.t.unpairedi(tdf, "income", "age")
}

}
