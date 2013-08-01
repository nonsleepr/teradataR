\name{dim.td.data.frame}
\alias{dim.td.data.frame}
\title{
Dimensions of a td data frame
}
\description{
Retrieve the dimension of a td data frame.
}
\usage{
\method{dim}{td.data.frame}(x)
}
\arguments{
  \item{x}{
  td data frame.
}
}
\details{
dim has a method which returns the totalRows and the length of the td.data.frame (as the numbers of rows and columns respectively).
}
\value{
It is NULL or a vector of mode integer. 
}
\examples{

\dontrun{
tdf <- td.data.frame("customer")
dim(tdf)
#returns 
#[1] 1000 18
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
