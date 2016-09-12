\name{LTRIM}
\alias{LTRIM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper Function LTRIM
}
\description{
Makes a wrapper around the fastpath function LTRIM
}
\usage{
LTRIM(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a teradata dataframe that contains column(s) of characters whose padding to their left will be trimmed
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A teradata data frame of the columns that will be trimmed at the beginning of each character
%% ...
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
tdf["col2"] <- LTRIM(tdf["col1"]) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
