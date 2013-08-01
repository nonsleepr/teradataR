\name{td.sample}
\alias{td.sample}
\title{
Sample Rows
}
\description{
Function to allow table a sample from a Teradata table.
}
\usage{
td.sample(tdf, sizes = missing, otable = "", odatabase = "")
}
\arguments{
  \item{tdf}{
    td data frame.
}
  \item{sizes}{
    size specification for the sample or samples you'd like to take.
}
  \item{otable}{
    output table name.
}
  \item{odatabase}{
    output database name.
}
}
\details{
Function can draw one or more samples from a td data frame table.  Sizes can be
integer or decimal values under 1.  With multiple samples you will get another
column called sampleid so you can distinguish.  If no output table is specified
you will get a data frame with your samples back.
}
\value{
td data frame pointing new sample table.
}
\examples{

\dontrun{
td.sample(tdf,1000,"newSample")
}

}
