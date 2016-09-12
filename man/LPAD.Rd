\name{LPAD}
\alias{LPAD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper Function LPAD
}
Makes a wrapper around the fastpath function LPAD
}
\usage{
LPAD(x, ilength, fill_string = " ")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a teradata dataframe that contains column(s) of characters that will be padded
}
  \item{ilength} {
the amount of padding to append to the beginning of the character  
}
  \item{fill_string} {
the character used to pad the the column(s) of characters that are passed into the function.  Default character is the empty character
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A teradata data frame of the columns that have been padded at the beginning
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
tdf["col2"] <- LPAD(tdf["col1", 15, " "]) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
