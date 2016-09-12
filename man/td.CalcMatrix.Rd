\name{td.CalcMatrix}
\alias{td.CalcMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper function for CalcMatrix
}
\description{
Creates A wrapper around the CalcMatrix table operator, using R Code.
}
\usage{
td.CalcMatrix(selectPhrase=string, ons=string, phase=NULL, calctype=NULL, output=NULL, null_handling=NULL, optional_operators=NULL, as=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{selectPhrase}{
 a character that represents the select clause for a query expression
}
  \item{ons} {
a character or list representation of the needed ON Clauses 
}
  \item{phase} {
the character representation of the input of the optional PHASE clause  
}
  \item{calctype} {
the character representation of the input of the optional CALCTYPE clause  
}
  \item{output} {
the character representation of the input of the optional OUTPUT clause  
}
  \item{null_handling} {
the character representation of the input of the optional specfication for null handling  
}
  \item{optional_operators} {
the character representation of the other operators that can be specified  
}
  \item{as} {
creates and alias  
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a character representation of a query that can be passed into TdQuery to use the CalcMatrix table operator.
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
on2 <- on(target= "select * from tab1") 
query <- td.CalcMatrix(selectPhrase="select session as ampkey, D1.* from TD_SYSFNLIB.calcmatrix", ons=on1, phase="LOCAL", as="D1")
res <- tdQuery(query)
print(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
