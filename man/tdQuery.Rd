\name{tdQuery}
\alias{tdQuery}
\title{
Query Teradata Database
}
\description{
Send a query to the Teradata Database and get a data frame back.  This call is a
wrapper of the sqlQuery function using the global tdConnection property.
}
\usage{
tdQuery(q, ...)
}
\arguments{
  \item{q}{
    An sql query.
}
  \item{\dots}{
    dots to pass along to sqlQuery for other attributes
}
}
\details{
This function allows the user to query the Teradata system without always having
to specify the connection.  It uses the global connection tdConnection set in
tdConnect.
}
\value{
data frame result for row returning queries.  "No Data" returned for non row
returning queries.
}
\examples{

\dontrun{

df <- tdQuery("SELECT * FROM customer")
#returns data frame object with all rows and columns from customer table

}
}
