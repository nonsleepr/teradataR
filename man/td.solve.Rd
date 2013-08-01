\name{td.solve}
\alias{td.solve}
\title{
Solve a system of equations
}
\description{
Function solves a system of equations via a matrix.
}
\usage{
td.solve(tdf, rhscols)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{rhscols}{
    vector of column names to solve for against the other matrix columns.
}
}
\details{
This function produces a correlation matrix from the columns that make up the
td data frame.  It then uses the 'solve' function in R to solve for the
right hand side columns based on that matrix.
}
\value{
vector or matrix of solved coefficients.
}
\examples{

\dontrun{
td.solve(tdf, "churn")
}

}
