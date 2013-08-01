\name{td.quantiles}
\alias{td.quantiles}
\title{
Quantile Values
}
\description{
Function to retrieve quantile values for td data frame and column.
}
\usage{
td.quantiles(tdf, col, qType = c("ter", "quar", "dec"))
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{col}{
    column name.
}
  \item{qType}{
    which type of quantile to bring back.  Valid are ("ter", "quar", "dec") for
    tertiles, quartiles and deciles respectively.
}
}
\details{
Gather quantile information from td data frame on this column.
}
\value{
data frame results of quantile values.
}
\examples{

\dontrun{
td.quantiles(tdf,"age")
}

}
