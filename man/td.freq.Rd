\name{td.freq}
\alias{td.freq}
\title{
Frequency Analysis
}
\description{
Produce a frequency analysis of a column in a td data frame.
}
\usage{
td.freq(tdf, col, plot = TRUE, ...)
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{col}{
    column name.
}
  \item{plot}{
    whether or not to plot results.  Default is TRUE.
}
  \item{\dots}{
    commands to pass on to plot if needed.
}
}
\details{
If td data frame consists of only 1 column then that column is used for the
frequency.  If 'col' is supplied with a multi column td data frame then that
column is used to get frequency values.
}
\value{
data frame of frequency results.
}
\examples{

\dontrun{
td.freq(tdf[3])
td.freq(tdf["age"])
td.freq(tdf, "age")
}

}
