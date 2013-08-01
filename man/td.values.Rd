\name{td.values}
\alias{td.values}
\title{
Values
}
\description{
Helps determine the nature and overall quality of the data
}
\usage{
td.values(tdf, col, group.by=NULL)
}
\arguments{
  \item{tdf}{
    td data frame
}
  \item{col}{
    column name 
}
  \item{group.by}{
    td data frame of columns that are used to group your results.
}
}
\details{
Given a td data frame and the name of a column, the td.values analysis provides 
rows with non-null values, rows with value 0, rows with a positive value, 
rows with a negative value, number of unique values and the number of rows 
containing blanks in the given column based on the type of column.
}
\value{
Single row data frame with the values results
}
\examples{

\dontrun{
td.values(tdf,"income")
td.values(tdf[3])
}

}
