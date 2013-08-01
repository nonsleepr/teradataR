\name{td.binomial}
\alias{td.binomial}
\title{
Binomial Test
}
\description{
Function to run binomial test against columns in Teradata.
}
\usage{
td.binomial(tdf, first, second, thresh = 0.05)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{first}{
    first column name.
}
  \item{second}{
    second column name.
}
  \item{thresh}{
    threshold value.  Defaults to 0.05.
}
}
\details{
The data for a binomial test is assumed to come from n independent trials, and have 
outcomes in either of two classes. The other assumption is that the probability of 
each outcome of each trial is the same, designated p. The values of the outcome could 
come directly from the data, where the value is always one of two kinds.
}
\value{
data frame with one row of binomial results.
}
\note{
Several statistical functions like this one use the Teradata Warehouse Miner
statistical tables and they need to be installed to use this function.  You can
also set up tdMetadataDB once you know where they are installed otherwise they
will be searched for.
}

\examples{

\dontrun{
td.binomial(tdf, "income", "age")
}

}
