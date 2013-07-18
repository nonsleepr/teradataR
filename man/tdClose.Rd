\name{tdClose}
\alias{tdClose}
\title{
Close connection
}
\description{
Close connection to database.
}
\usage{
tdClose(x = tdConnection)
}
\arguments{
  \item{x}{
    connection to close.  Defaults to tdConnection.
}
}
\details{
Close the connection object.  tdConnection is a global variable and is the default
connection to close.
}
\examples{

\dontrun{
tdClose()
}
}
