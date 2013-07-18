\name{td.sigmoid}
\alias{td.sigmoid}
\title{
Sigmoid Transformation
}
\description{
Function to output a sigmoid transformation for a column in a td data frame.
}
\usage{
td.sigmoid(tdf, col, oTable, oDatabase = "", sigmoidType = "logit")
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
  \item{sigmoidType}{
    type of sigmoid.  Valid values are ("logit","mlogit",and "tanh").
}
}
\details{
Create table of the sigmoid tranformation using the type on this td data frame 
and column.
}
\value{
td data frame pointing to the new table produced.
}
\examples{

\dontrun{
td.sigmoid(tdf,"age","newSigTable")
}

}
