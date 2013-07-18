\name{tdConnect}
\alias{tdConnect}
\title{
Connect to Teradata database
}
\description{
Open up a channel to a Teradata database.
}
\usage{
tdConnect(dsn, uid = "", pwd = "", database = "", dType = "odbc")
}
\arguments{
  \item{dsn}{
    string containing the data source name to connect to.
}
  \item{uid}{
    string containing the user id.
}
  \item{pwd}{
    string containing the password.
}
  \item{database}{
    string containing the default database.
}
  \item{dType}{
    string containing the driver type: odbc(default) or jdbc
}
}
\details{
Function uses RODBC to make a connection to a Teradata database.
}
\value{
returns an R channel
}
\examples{

\dontrun{
tdConnect("myDsn")
}

}
