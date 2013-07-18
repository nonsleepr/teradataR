\name{td.t.unpaired}
\alias{td.t.unpaired}
\title{
T Test Unpaired
}
\description{
Function to run unpaired T Test
}
\usage{
td.t.unpaired(tdf, col1, col2, thresh = 0.05)
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
The unpaired t test is similar to the paired test, but there is no correspondence between 
values of the samples. It assumes within each sample, values are identically distributed 
normal random variables, and that the two samples are independent of each other.
}
\value{
Single row data frame with the T Test results
}
\examples{

\dontrun{
td.t.unpaired(tdf, "income", "age")
}

}
