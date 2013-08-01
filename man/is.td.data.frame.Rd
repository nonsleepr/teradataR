\name{is.td.data.frame}
\alias{is.td.data.frame}
\title{
Is an Object a Teradata Data Frame
}
\description{
Check to see if object has a td.data.frame in its class
}
\usage{
is.td.data.frame(x)
}
\arguments{
  \item{x}{
    any R Object.
}
}
\value{
is.data.frame returns TRUE if its argument is a data frame (that is, it has "td.data.frame" amongst its classes) and FALSE otherwise. 
}
\examples{
is.td.data.frame(iris)
#FALSE because iris is a data frame
}
