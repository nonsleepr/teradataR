\name{td.zscore}
\alias{td.zscore}
\title{
Zscore Transformation
}
\description{
Function to create a column as a zscore transformation.
}
\usage{
td.zscore(tdf, col, oTable, oDatabase = "")
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{col}{
    column to test.
}
  \item{oTable}{
    output table name.
}
  \item{oDatabase}{
    output database name.
}
}
\details{
In a Z-Score transformation, a numeric column is transformed into its Z-score based 
on the mean value and standard deviation of the data in the column. It transforms 
each column value into the number of standard deviations from the mean value of the column.
}
\value{
returns a td data frame pointing to your new table created with your new column as the
z score transformation.
}
\examples{

\dontrun{
td.zscore(tdf,"age","newCust")
}

}
