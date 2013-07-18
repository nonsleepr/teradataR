\name{td.data.frame}
\alias{td.data.frame}
\title{
Teradata Data Frames
}
\description{
An R object that represents a Teradata table.  The object initializes with certain
information upon creation such as total number of rows and what the column names of
the table are.
}
\usage{
td.data.frame(table, database = "")
}
\arguments{
  \item{table}{
    name of table in Teradata to point to.
}
  \item{database}{
    database where table resides.  Defaults to current DATABASE on connection.
}
}
\details{
This is the main object that stores the link between a Teradata table and the
R environment.  No data from the table is actually resident in the R system only
some initial values of rows and columns are persisted and stored within the
td data frame object.
}
\value{
Returns a td data frame.
}
\examples{
\dontrun{

tdf <- td.data.frame("customer")
#pointer to customer table in current database
tdf2 <- td.data.frame("customer", "otherDB")
#pointer to customer table in database 'otherDB'

}
}
