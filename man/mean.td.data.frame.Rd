\name{mean.td.data.frame}
\alias{mean.td.data.frame}
\title{
Arithmetic Mean
}
\description{
Function to calculate the arithmetic mean.
}
\usage{
\method{mean}{td.data.frame}(x, \dots)
}
\arguments{
  \item{x}{
    td data frame.
}
  \item{\dots}{
    For generic consistency.
}
}
\details{
Means for all numeric columns in the td data frame are returned as well as
NA values for the non numeric columns.
}
\value{
vector of mean values for each column.
}
\examples{

\dontrun{
mean(tdf)  #tdf is a td data frame
}

}
