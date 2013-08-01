\name{td.dagostino.pearson}
\alias{td.dagostino.pearson}
\title{
D'Agostino Pearson Test
}
\description{
Function to use D'Agostino Pearson Test
}
\usage{
td.dagostino.pearson(tdf, col, thresh = 0.05)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{col}{
    column name.
}
  \item{thresh}{
    threshold value.  Defaults to 0.05.
}
}
\details{
Run the D'Agostino Pearson test against the td data frame for a column.
}
\value{
data frame with one row of D'Agostino Pearson results.
}
\note{
Several statistical functions like this one use the Teradata Warehouse Miner
statistical tables and they need to be installed to use this function.  You can
also set up tdMetadataDB once you know where they are installed otherwise they
will be searched for.
}

\examples{

\dontrun{
td.dagostino.pearson(tdf,"income")
}

}
