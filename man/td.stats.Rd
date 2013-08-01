\name{td.stats}
\alias{td.stats}
\title{
General Statistics
}
\description{
This function uses the td data frame object passed in and retrieves some
statistics for a particular column.
}
\usage{
td.stats(tdf, col, which = "all", type = "population", group.by=NULL)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{col}{
    name of column.
}
  \item{which}{
    string or array of which statistics to return.  Any of the follow are valid
    choices: ("cnt","min","max","mean","sum","uss","css","var","std","skew",
              "kurtosis","stderr","cvar").  You can also use "all" to retrieve them all.
}
  \item{type}{
    whether to return the statistic based on the population or sample
}
  \item{group.by}{
    td data frame of columns that are used to group your results.
}
}
\details{
This function collects certain statistics from the Teradata table and returns them
to the user via an R data frame.  Count, minimum, maximum, mean, sum, uncorrected
sum of squares, corrected sum of squares, variance, standard deviation, skewness,
kurtosis, standard error, and coefficient of variance are statistics to choose from.
You can also put a type of "all" to retrieve them all at once.
}
\value{
data frame object with 1 row and the stats you have selected
}
\examples{

\dontrun{
td.stats(tdf, "income", c("min","max","mean"))
}

}
