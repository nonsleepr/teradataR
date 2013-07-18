\name{td.rank}
\alias{td.rank}
\title{
Rank
}
\description{
Function returns list of ranks from column.
}
\usage{
td.rank(tdf, col)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{col}{
    column name.
}
}
\details{
Return several rank values from the td data frame for this column.
}
\value{
data frame of one row with rank values.
}
\examples{

\dontrun{
td.rank(tdf,"age")
}

}
