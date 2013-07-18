\name{td.rescale}
\alias{td.rescale}
\title{
Rescale Values of Column
}
\description{
Function rescaled values of a td data frame column.
}
\usage{
td.rescale(tdf, col, oTable, oDatabase = "", lower = 0, upper = 1)
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
  \item{lower}{
    lower bound to rescale to.
}
  \item{upper}{
    upper bound to rescale to.
}
}
\details{
Create a new output table of a column in this td data frame.  This function 
allows you to rescale a column.
}
\value{
td data frame pointing to new output table.
}
\examples{

\dontrun{
td.rescale(tdf,"age","newTable")
}

}
