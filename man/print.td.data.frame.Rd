\name{print.td.data.frame}
\alias{print.td.data.frame}
\title{
Show contents of a td data frame
}
\description{
Display information about the td data frame such as database, tablename, and
column names.
}
\usage{
\method{print}{td.data.frame}(x)
}
\arguments{
  \item{x}{
    td data frame.
}
}
\details{
Function outputs specific information about the current table pointed at
by this td data frame.  Database name, table name, columns and the row count
are all pieces of information that is output.
}
\value{
A list of the information
}
\examples{

\dontrun{
tdf    #a td data frame object
}

}
