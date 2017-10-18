\name{INITCAP}
\alias{INITCAP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper Function INITCAP
}
\description{
Makes a wrapper around the fastpath function INITCAP
}
\usage{
INITCAP(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a teradata dataframe that contains columns of characters that will be capitalized
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A teradata data frame of the columns that have their first letters capitalized
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Todd Brye, Erin Cole
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
tdf["col2"] <- INITCAP(tdf["col1"]) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
