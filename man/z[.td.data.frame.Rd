\name{[.td.data.frame}
\alias{[.td.data.frame}
\title{
Extract Teradata Data Frame
}
\description{
Allow manipulation and extraction of a td data frame.
}
\usage{
\method{[}{td.data.frame}(x, i, j)
}
\arguments{
  \item{x}{
    td data frame.
}
  \item{i}{
    element to extract
}
  \item{j}{
    element to extract
}
}
\details{
This overloaded function operates almost exactly like a data frame extract.  It
can be used to reduce the number of columns by using the index values.  One 
noticeable difference is that a td data frame has no actual data in the client.
This means that row operations don't extract certain rows like a data frame object.
You must use td.sample if you wish to alter table sizes.
}
\value{
td data frame of new column dimensions.
}
\examples{

\dontrun{
tdf[3]  #returns td data frame with just column 3
tdf["age"]  #returns td data frame with column age
}

}
