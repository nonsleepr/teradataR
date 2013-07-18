\name{td.bincode}
\alias{td.bincode}
\title{
Create Table of Bincode Values
}
\description{
Generate a Teradata table of columns and the newly bincoded column.
}
\usage{
td.bincode(tdf, col, oTable, oDatabase = "", bins = 10)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{col}{
    name of column to be bincoded.
}
  \item{oTable}{
    output table name.
}
  \item{oDatabase}{
    output database name.
}
  \item{bins}{
    number of groups to bin(break) column into.
}
}
\details{
Create a new table with the same column name as the input but now with bincoded
values.
}
\value{
td data frame that represents the new table created by bincode.
}
\examples{

\dontrun{
td.bincode(tdf, "age", "newBinTable", bins=12) # break age into 12 groups or bins
}

}
