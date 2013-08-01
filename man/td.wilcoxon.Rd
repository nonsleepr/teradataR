\name{td.wilcoxon}
\alias{td.wilcoxon}
\title{
Wilcoxon Test
}
\description{
The Wilcoxon Signed Ranks Test is an alternative analogous to the t-test
for correlated samples.
}
\usage{
td.wilcoxon(tdf, col1, col2, thresh = 0.05)
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
Given a td data frame and names of paired numeric columns, a Wilcoxon test is produced.
The Wilcoxon tests whether a sample comes from a population with a specific mean or median.
The null hypothesis is that the samples come from populations with the same mean or median.
The alternative hypothesis is that the samples come from populations with different means
or medians (two-tailed test), or that in addition the difference is in a specific direction
(upper-tailed or lower-tailed tests). Output is a p-value, which when compared to the
users threshold, determines whether the null hypothesis should be rejected.
}
\value{
Single row data frame with the Wilcoxon test results
}
\examples{

\dontrun{
td.wilcoxon(tdf, "income", "age")
}

}
