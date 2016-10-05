\name{on}
\alias{on}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
On Function
}
\description{
Creates A representation of an ON Clause in an ExecR Table Operator query.
}
\usage{
on(target=NULL, from=NULL, subQuery=NULL, partition=NULL, hash=NULL, order=NULL, local_order=NULL, null_order=NULL, dimension=NULL, as=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{target}{
a character that represents a name of a table or a query expression
}
  \item{from} {
a character that represents a name of the origin of a query expression
}
  \item{subQuery}{
a character that represents a nested ON Clause or nested query expression that is part of an ON Clause
}
  \item{partition} {
the parallel option, partition by <column>, or partition by <any>  
}
 \item{hash} {
the parallel option, hash by <column>
}
  \item{order}{
the parallel option, order by <column> 
}
  \item{local_order}{
the parallel option, local order by <column>
}
  \item{null_order}{
specification for the parallel option local_order 
}
  \item{dimension} {
the parallel option, dimension 
}
  \item{as} {
creates and alias    
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a string representation of an ON Clause for a table operator
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Erin Cole
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
o <- on(target="select *" from="tab1", partition="any")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
