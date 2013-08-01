\name{td.recode}
\alias{td.recode}
\title{
Recode
}
\description{
Function to allow change values of a column to new values.
}
\usage{
td.recode(tdf, col, oTable, oDatabase = "", codes, other="same")
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{col}{
    column name.
}
  \item{oTable}{
    output table name.
}
  \item{oDatabase}{
    output database name.
}
  \item{codes}{
    vector of value pairs.  Pairs are current value followed by new value.
    Must be even number of elements in vector.
}
  \item{other}{
    what to do with values not in the codes vector.  Valid are
    ("same", "null").  Values not in codes vector stay same or are null.
}
}
\details{
Create a new output table in Teradata with the recoded column from this
td data frame.
}
\value{
td data frame pointer to the new table created.
}
\examples{

\dontrun{
td.recode(tdf,"marital_status","newCustomer",codes=c("1","S","2","M"))
}

}
