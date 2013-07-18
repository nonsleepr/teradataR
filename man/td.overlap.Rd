\name{td.overlap}
\alias{td.overlap}
\title{
Overlap
}
\description{
Function returns data frame which shows how the fields from table 1 overlap
table 2's fields.
}
\usage{
td.overlap(tdf1, tdf2, fields1, fields2)
}
\arguments{
  \item{tdf1}{
    td data frame of table one.
}
  \item{tdf2}{
    td data frame of table two.
}
  \item{fields1}{
    fields to use for overlap from table one.
}
  \item{fields2}{
    fields to use for overlap from table two.
}
}
\details{
Match up columns and see which values are contained in both tables, how many
uniques there are between each other.
}
\value{
results showing a table of the overlapping values.
}
\examples{

\dontrun{
td.overlap(tdf, tdf2, c("i1","i2"), c("f1","f2")) # fields can be same or diff
}

}
