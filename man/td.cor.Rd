\name{td.cor}
\alias{td.cor}
\title{
Correlation Matrix
}
\description{
Create a matrix of the correlation between numeric columns.
}
\usage{
td.cor(tdf)
}
\arguments{
  \item{tdf}{
    td data frame.
}
}
\details{
Build a matrix of all columns in the td data frame.
}
\value{
Return a matrix of numeric correlation values.
}
\examples{

\dontrun{
td.cor(tdf[1:4])  #select only the numeric columns
} 

}
