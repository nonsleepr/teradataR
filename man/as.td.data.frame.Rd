\name{as.td.data.frame}
\alias{as.td.data.frame}
\title{
Coerce to a td data frame
}
\description{
Coerce a data frame into a td data frame
}
\usage{
as.td.data.frame(x, ...)
}
\arguments{
  \item{x}{
data frame.
}
}
\details{
If a data frame is supplied a new table is created on the Teradata system in the
current database.  The new table has the same name as the data frame variable in
R.
}
\value{
as.td.data.frame return a td.data.frame
}

\examples{

\dontrun{as.td.data.frame(iris)}
#Builds a table named iris in Teradata

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
