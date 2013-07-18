\name{tdMetadataDB}
\alias{tdMetadataDB}
\title{
Set metadata database
}
\description{
Function to set location of statistic tables.
}
\usage{
tdMetadataDB(database)
}
\arguments{
  \item{database}{
    name of database where metadata like statistics tables will be found.
}
}
\details{
This function sets a global variable 'tdMetadata' which is used for certain
statistical tests.  When this value isn't present other functions will try
and search for the tables in your Teradata system.  This function helps
reduce search queries.  Sending a NULL value will remove this variable from
the global list and clear it.
}
\value{
Returns the database name if set; otherwise NA.
}
\examples{

\dontrun{
tdMetadataDB("MetaDB")
}

}
