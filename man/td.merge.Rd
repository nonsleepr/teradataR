\name{td.merge}
\alias{td.merge}
\title{
Merge Rows of Teradata Tables
}
\description{
Function to merge rows of two Teradata tables based on merge type.
}
\usage{
td.merge(tdf1, tdf2, oTable = "newMergeTable", oDatabase = "", mergeType = "union")
}
\arguments{
  \item{tdf1}{
    td data frame of table one.
}
  \item{tdf2}{
    td data frame of table two.
}
  \item{oTable}{
    output table name.
}
  \item{oDatabase}{
    output database name.
}
  \item{mergeType}{
    type of merge to perform.  Values can be ("union","minus",and "intersect")
}
}
\details{
Create a new output table based on the merge type between two td data frames.
Each td data frame must consist of the same number of columns in order to merge
correctly.
}
\value{
td data frame pointing to the output table in Teradata.
}
\examples{

\dontrun{
td.merge(tdf1, tdf2, "newTable")
}

}
