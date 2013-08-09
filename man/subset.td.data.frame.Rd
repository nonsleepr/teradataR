\name{subset.td.data.frame}
\alias{subset.td.data.frame}
\title{
Subsetting a Teradata Data Frame
}
\description{
Return subsets of teradata data frames which meet conditions.
}
\usage{
\method{subset}{td.data.frame}(x, subset, select, drop = FALSE, \dots)
}
\arguments{
  \item{x}{
    td data frame to be subset.
}
  \item{subset}{
    logical expression indicating rows to keep.
}
  \item{select}{
    expression, indicating columns to select from a teradata data frame.
}
  \item{drop}{
    passed on to [ indexing operator.
}
  \item{\dots}{
    Allow other commands to be sent to plot.
}
}
\details{
This is a generic function, with methods supplied for teradata data frames.

The subset argument works on the rows. Note that subset will be evaluated in the teradata data frame, so columns can be referred to (by name) as variables in the expression (see the examples).

It works by first replacing column names in the selection expression with the corresponding column numbers in the teradata data frame and then using the resulting integer vector to index the columns. This allows the use of the standard indexing conventions so that for example ranges of columns can be specified easily, or single columns can be dropped (see the examples).

}
\value{
An object similar to x but which adds the subset condtion which is used when accessing the real Teradata table.
}
\examples{

\dontrun{
subset(tdf, age >= 65, select = income:married)
subset(tdf, gender == 'F', select = -age)
}

}
