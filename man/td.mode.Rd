\name{td.mode}
\alias{td.mode}
\title{
Mode Value of Column
}
\description{
Function to find the most frequent value of a column in a td data frame.
}
\usage{
td.mode(tdf, col)
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
Function goes out and gets the most frequent value or mode of a column in a
td data frame and returns it.
}
\value{
specific column value for most frequent value.
}
\examples{

\dontrun{
td.mode(tdf,"age")
td.mode(tdf[3])
}

}
