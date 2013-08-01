\name{td.nullreplace}
\alias{td.nullreplace}
\title{
Replace Null Values
}
\description{
Function which replaces null values within a td data frame and column.
}
\usage{
td.nullreplace(tdf, col, oTable, oDatabase = "", rType = "literal", rVal = "0")
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
  \item{rType}{
    type of replacement.  Valid values are ("literal","mean","mode", and "median").
}
  \item{rVal}{
    value for literal replacement.
}
}
\details{
Build new table with column name after replacement is done via rType.
}
\value{
td data frame pointing to new Teradata table with NULLs replaced.
}
\examples{

\dontrun{
td.nullreplace(tdf,"age","newReplaceTable", rVal="16")
}

}
